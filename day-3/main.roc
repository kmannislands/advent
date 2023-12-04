app "day-2-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "test.txt" as sample : List U8,
    ]
    provides [main] to pf

# Take-aways from day 2:
# - Understand the type system better. Complex code gets really out of hand without declarations
# - Keep up the testing, that was good.

## From Day 1: Given as ASCII byte, return whether its a valid digit
isDigit = \charByte -> Bool.and ('0' <= charByte) (charByte <= '9')
isPeriod = \charByte -> charByte == '.'
isNewline = \charByte -> charByte == '\n'

# Let's start by writing the high level reducer
ParserState : {
    col: U8,
    line: U8,
    digitBytes: List U8,
    numbers: List U32,
    # symbols: Set List Nat
}

initialState : ParserState
initialState = {
    col: 0,
    line: 0,
    digitBytes: [],
    numbers: []
}

reduce: ParserState, U8 -> ParserState
reduce = \state, byte ->
    if isDigit byte then
        # Push to digitBytes
        state
    else
        # If there are digitsBytes, finish them and push a number regardless of what comes next
        if isPeriod byte then
            # Advance line
            state
        else if isNewline byte then
            # Advance column, reset line
            state
        else 
            # We have a character, add its coordinates to the set
            state

main =
    parsed = List.walk sample initialState reduce

    dbg parsed

    Stdout.line "All done!"