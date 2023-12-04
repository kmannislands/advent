app "day-2-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "input.txt" as sample : Str,
    ]
    provides [main] to pf

# We're going to implement a parser for the game state using a parser combinator pattern Parser combinators
# work well in functional paradigms

# Parsers in parser combinators operate on a 'context', returning a new context after each parse whether
# successful or failed This context should store line/column information for nice error reporting (more
# relevant if you're implementing something more complicated like a PL) In this case, we'll store the whole
# "program" (in this case the game result) string and keep track of line/column (col) information
advance = \ctx, chars ->
    # TODO: wrap lines
    { ctx& col: ctx.col + chars }

lit = \litStr ->
    litLen = Str.countUtf8Bytes litStr
    \ctx ->
        # TODO this could be faster with a walkScalarsUntil
        subStr = Str.toUtf8 ctx.prog
            |> List.dropFirst ctx.col
            |> Str.fromUtf8
            # Seems fine not to handle the utf8 conversion error here since we just got the bytes from a str
            |> Result.withDefault ""
        matched = Str.startsWith subStr litStr 
        if matched then
            Token { value: 0, ctx: advance ctx litLen }
        else
            ParseError ctx "Couldn't find expected lit '\(litStr)' at offset \(Num.toStr ctx.col)"

# Basic test
expect
    ctx = progCtx "Game 1"
    parsedToken = ctx |> (lit "Game")
    parsedToken == Token { value: 0, ctx: advance ctx 4 }

# It looks at the current context correctly
expect
    ctx = progCtx "foobar"
    parsedToken = (advance ctx 3) |> (lit "bar")
    parsedToken == Token { value: 0, ctx: advance ctx 6 }

## From Day 1: Given as ASCII byte, return whether its a valid digit
isDigit = \charByte -> Bool.and ('0' <= charByte) (charByte <= '9')
# We'll also want to know when a UTF8 code point byte is whitespace. For now, let's just worry about ` `
isWhiteSpace = \charByte -> charByte == ' '

# Also from day 1. Note that we don't perform any safety check. Inputs are assumed to already have gone
# through isDigit. TODO: This could be improved by checking for overflows and using tagged returns
byteToInt = \byte -> Num.toU32 (byte - '0')
bytesToU32 = \digitBytes ->
    len = List.len digitBytes
    List.walkWithIndex digitBytes 0 \num, byte, i ->
        byteDigitValue = byteToInt byte
        power = Num.toU32 (len - 1 - i)
        mult = Num.powInt 10 power
        num + (mult * byteDigitValue)

expect bytesToU32 ['1'] == 1
expect bytesToU32 ['1', '2'] == 12

uint32 = \_ ->
    \ctx ->
        # Walk the program as bytes, skipping white space, accumulating character code points if they are a
        # digit to later convert to an actual roc number and stopping if we encounter a character that
        # couldn't be part of a number
        progBytes = Str.toUtf8 ctx.prog |> List.dropFirst ctx.col
        { numberBytes, bytesEaten } = List.walkUntil progBytes { numberBytes: [], bytesEaten: 0 } \state, byte ->
            if isWhiteSpace byte then
                # "Trim" whitespace off the start of a number but end if we have already started parsing a num
                if (List.len state.numberBytes) == 0 then
                    Continue { state& bytesEaten: state.bytesEaten + 1 }
                else
                    Break state
            else if isDigit byte then
                Continue { bytesEaten: state.bytesEaten + 1, numberBytes: List.append state.numberBytes byte }
            else
                Break state

        # If we got some number bytes go ahead and convert them Note this doesn't check for U32 overflow and
        # should be improved by using Num.checks
        when numberBytes is
            [..] ->
                uint32Val = bytesToU32 numberBytes
                newCtx = advance ctx bytesEaten
                Token { value: uint32Val, ctx: newCtx }
            _ -> ParseError ctx "Couldn't parse a number"

progCtx = \prog -> { prog: prog, line: 0, col: 0 }

## Simplest case -- pre-trimmed
expect 
    ctx = progCtx "23"
    parsedToken = ctx |> (uint32 {})
    parsedToken == Token { value: 23, ctx: advance ctx 2 }

## Check that we trim leading whitespace when parsing a number
expect
    ctx = progCtx "  420"
    parsedToken = ctx |> (uint32 {})
    parsedToken == Token { value: 420, ctx: advance ctx 5 }

## Check that we stop eating chars when there's a non digit
expect
    ctx = progCtx "111:"
    parsedToken = ctx |> (uint32 {})
    parsedToken == Token { value: 111, ctx: advance ctx 3 }

expect
    ctx = progCtx "2 3"
    parsedToken = ctx |> (uint32 {})
    parsedToken == Token { value: 2, ctx: advance ctx 1 }

## Now let's write out first combinator to represent tokens in order
##
## We want it to look like:
##
## gameId = seq [
##    (lit "Game")m
##    (uint32 {})
##    (lit ":")
## ]

seq = \parsers, mapResult ->
    \ctx ->
        init = { errored: Bool.false, parsed: [], error: ParseError ctx "No seq" }
        seqResult = List.walkUntil parsers init \state, parser ->
            currentCtx = when List.last state.parsed is
                Ok lastParsed -> lastParsed.ctx
                _ -> ctx
            parseResult = parser currentCtx
            when parseResult is
                ParseError errorCtx errorMsg ->
                    Break { state& errored: Bool.true, error: ParseError errorCtx errorMsg }
                Token result ->
                    Continue { state& parsed: List.append state.parsed result }
        if seqResult.errored then
            seqResult.error
        else
            when List.last seqResult.parsed is
                Ok lastParsed -> mapResult seqResult.parsed lastParsed.ctx
                _ -> crash "Parser Combinator declaration error: parsed without error yet got an empty sequence"

gameIdParser = seq
    [
        (lit "Game"),
        (uint32 {}),
        (lit ":"),
    ]
    \parsedTokens, lastCtx ->
        when List.get parsedTokens 1 is
            Ok v -> Token { value: v.value, ctx: lastCtx  }
            _ -> crash "Invariant violated: parsed game id but didn't have a valid gameid token"
        
    # |> map \{ value, ctx } ->
    #     # Grab just the game id part, throwing out everything else

expect
    ctx = progCtx "Game 420:"
    parsedToken = gameIdParser ctx
    parsedToken ==  Token {
        value: 420,
        ctx: advance ctx 9
    }

parseGameId = \gamePart ->
    when Str.replaceFirst gamePart "Game " "" |> Str.toU32 is
        Ok gameIdInt -> gameIdInt
        _ -> crash "Failed to parse gameId from partial line '\(gamePart)'"

emptyDraw = { red: 0, green: 0, blue: 0 }

parseDraws = \drawsPart ->
    drawStrs = Str.split drawsPart "; "
    List.map drawStrs \drawStr ->
        draws = Str.split drawStr ", "
        List.walk draws emptyDraw \result, colorResult ->
            when Str.split colorResult " " is
                [numberStr, colorStr] ->
                    drawForColor = when Str.toU32 numberStr is
                        Ok value -> value
                        _ -> crash "Couldn't parse out draw for color '\(colorStr)'"
                    when colorStr is
                        "red" -> { result& red: drawForColor}
                        "blue" -> { result& blue: drawForColor}
                        "green" -> { result& green: drawForColor}
                        _ -> crash "Unrecognized Color '\(colorStr)' in 'colorResult"
            
                _ -> crash "Couldn't parse number and count out of draw piece \(drawStr)"

parseGame = \gameLine ->
    gameLinePieces = Str.split gameLine ": "
    when gameLinePieces is
        [gamePart, drawsPart] -> { gameId: parseGameId gamePart, draws: parseDraws drawsPart }
        _ -> crash "Couldn't parse gameLine \(gameLine)"

cubesInBag = {
    red: 12,
    green: 13,
    blue: 14
}

isImpossibleGame = \game ->
    List.any game.draws \draw ->
        (draw.red > cubesInBag.red) || (draw.green > cubesInBag.green) || (draw.blue > cubesInBag.blue)

possibleGameIds = \gameResults ->
    List.walk gameResults [] \possibleSoFar, game ->
        if isImpossibleGame game then
            possibleSoFar
        else
            List.append possibleSoFar game.gameId

main =
    lines = Str.split sample "\n"
    gameResults = List.map lines \line -> parseGame line
    possibleGames = possibleGameIds gameResults
    sumOfImpossibleIds = List.walk possibleGames 0 \total, gameId -> total + gameId
    Num.toStr sumOfImpossibleIds |> Stdout.line 