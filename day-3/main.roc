app "day-2-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "input.txt" as sample : List U8,
    ]
    provides [main] to pf

# By now I've used roc for long enough to write a program that has tech debt.
# Some take-aways from day 2 to avoid that:
# - Understand the type system better. Complex code gets really out of hand without declarations
# - Keep up the testing, that was good.
# - Maybe today/tomorrow, figure out how to implement lib code in roc to share these functions

## From Day 1: Given as ASCII byte, return whether its a valid digit
isDigit = \charByte -> Bool.and ('0' <= charByte) (charByte <= '9')
isPeriod = \charByte -> charByte == '.'
isNewline = \charByte -> charByte == '\n'
isStar = \charByte -> charByte == '*'

# Also from day 1.
byteToInt = \byte -> Num.toU32 (byte - '0')
# From Day 2.
## Note that we don't perform any safety check. Inputs are assumed to already have gone
## through isDigit. TODO: This could be improved by checking for overflows and using tagged returns
bytesToU32 : List U8 -> U32
bytesToU32 = \digitBytes ->
    len = List.len digitBytes
    List.walkWithIndex digitBytes 0 \num, byte, i ->
        byteDigitValue = byteToInt byte
        power = Num.toU32 (len - 1 - i)
        mult = Num.powInt 10 power
        num + (mult * byteDigitValue)

# Let's start by writing the high level reducer

Number : {
    # The line that the number is on
    line: U8,
    # The column that the number started in
    col: U8,
    # How many characters did the number use
    length: U8,
    # Actual parsed value of the number to use in sum later
    value: U32
}

ParserState : {
    col: U8,
    line: U8,
    digitBytes: List U8,
    numbers: List Number,
    # Set of coordinates (line, col) that contain symbols
    otherSymbols: Set (List U8),
    gears: Set (List U8)
}

initialState : ParserState
initialState = {
    col: 0,
    line: 0,
    digitBytes: [],
    numbers: [],
    otherSymbols: Set.empty {},
    gears: Set.empty {}
}

## Update the state when we are done with a number (encounter a non-digit character).
## TODO: Update to store line/col snapshot
finishNumber : ParserState -> ParserState
finishNumber = \state ->
    countBytes = List.len state.digitBytes |> Num.toU8
    if countBytes > 0 then
        parsedNum = bytesToU32 state.digitBytes
        startCol = state.col - countBytes
        num = { line: state.line, col: startCol, length: countBytes, value: parsedNum }
        {
            state&
            digitBytes: [],
            numbers: List.append state.numbers num
        }
    else
        state


# Any more complicated and we'll need a state machine, eh?
reduce: ParserState, U8 -> ParserState
reduce = \state, byte ->
    if isDigit byte then
        # Push to digitBytes
        { state& digitBytes: List.append state.digitBytes byte, col: state.col + 1 }
    else
        # If there are digitsBytes, finish them and push a number regardless of what comes next
        # The fact that we've hit any non-digit char means we're done with the number
        newState = finishNumber state
        if isPeriod byte then
            # We don't care, just dvance col
            { newState& col: newState.col + 1 }
        else if isNewline byte then
            # Advance column, reset line
            { newState& line: newState.line + 1, col: 0 }
        else
            symbolCoord = [newState.line, newState.col]
            if isStar byte then
                gears = Set.insert newState.gears symbolCoord
                { newState& gears, col: newState.col + 1 }
            else
                # We have a non-gear, add its coordinates to the set
                otherSymbols = Set.insert newState.otherSymbols symbolCoord
                { newState& otherSymbols, col: newState.col + 1 }

ParseResult : {
    numbers: List Number,
    otherSymbols: Set (List U8),
    gears: Set (List U8),
}

parseSchematic : List U8 -> ParseResult
parseSchematic = \schematic ->
    parsed = List.walk schematic initialState reduce
    { numbers: parsed.numbers, otherSymbols: parsed.otherSymbols, gears: parsed.gears }

## Given the position information about a number, determine a list of coordinates to check for the presence of a symbol
## TODO: perf: we're checking digit cells and cells that go past the end of the input
symbolCoordCandidates : Number -> List (List U8)
symbolCoordCandidates = \num ->
    rowStart = Num.subChecked num.line 1 |> Result.withDefault 0
    # TODO: We're potentially going past the end of the input but that shouldn't crash
    rowEnd = num.line + 1

    rowsToCheck = List.range { start: At rowStart, end: At rowEnd }

    colStart = Num.subChecked num.col 1 |> Result.withDefault 0
    colEnd = num.col + num.length

    colsToCheck = List.range { start: At colStart, end: At colEnd }

    # cartesian product
    List.joinMap rowsToCheck \rowIdx ->
        List.map colsToCheck \colIdx ->
            [rowIdx, colIdx]

validPartNumbers : ParseResult -> List U32
validPartNumbers = \{ numbers, otherSymbols, gears } ->
    symbols = Set.union otherSymbols gears
    List.keepIf numbers \num ->
        coordinatesToCheck = symbolCoordCandidates num
        List.any coordinatesToCheck \coord ->
            Set.contains symbols coord
    |> List.map .value

expect
    candidates = symbolCoordCandidates { col: 0, line: 0, length: 1, value: 9 }
    candidates == [[0, 0], [0, 1], [1, 0], [1, 1]]

expect
    { numbers } = parseSchematic sample
    List.map numbers .value == [467, 114, 35, 633, 617, 58, 592, 755, 664, 598]

## Part II: We now need to know if the number is adjacent to a gear specifically.
## To do this, we should:
##  - keep track of whether a symbol is a gear
##  - when computing what numbers are adjacent to what symbols, also uniquely identify the symbol (through its coord)
##  - Perhaps we do this by building a dictionary like: Dict<GearCoord, List Number> { [0, 0]: [234, 567] }
main =
    parsed = parseSchematic sample

    dbg parsed

    partNumbers = validPartNumbers parsed

    dbg partNumbers

    sum = List.walk partNumbers 0 \runningSum, partNum -> runningSum + partNum

    Stdout.line "All done! Part numbers summed to \(Num.toStr sum)"