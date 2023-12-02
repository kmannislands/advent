app "day-2-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "input.txt" as sample : Str,
    ]
    provides [main] to pf

# Ctx : {
#     line: U8,
#     col: U8,
#     prog: Str,
# }

# advance: (Ctx, Int) -> Ctx
# TODO: wrap lines
advance = \ctx, eatStr ->
    chars = Str.toUtf8 eatStr |> List.len 
    { ctx& col: ctx.col + chars }

uint32 = \_ ->
    \ctx ->
        # This ain't right
        trimmed = Str.trim ctx.prog
        parseResult = trimmed |> Str.toU32
        when parseResult is
            Ok uint32Val ->
                newCtx = advance ctx trimmed
                TokenNumber { value: uint32Val, ctx: newCtx }
            _ -> ParseError ctx

progCtx = \prog -> { prog: prog, line: 0, col: 0 }

## Simplest case -- pre-trimmed
expect 
    ctx = progCtx "23"
    parsedToken = ctx |> (uint32 Empty)
    parsedToken == TokenNumber { value: 23, ctx: advance ctx "23" }

parseGameId = \gamePart ->
    when Str.replaceFirst gamePart "Game " "" |> Str.toU32 is
        Ok gameIdInt -> gameIdInt
        _ -> crash "Failed to parse gameId from partial line '\(gamePart)'"

emptyDraw = { red: 0, green: 0, blue: 0 }
# Draw : {
#     red: Num U8,
#     blue: Num U8,
#     green: Num U8,
# }
# parseDraws: Str -> List Draw
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