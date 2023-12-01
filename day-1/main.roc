app "day-1-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "input.txt" as sample : List U8,
    ]
    provides [main] to pf

# https://www.ascii-code.com/
isDigit = \baseTenByte -> Bool.and (47 < baseTenByte) (baseTenByte < 58)
isLineSep = \baseTenByte -> baseTenByte == 10

asciiToDigit: Int a -> Int a
asciiToDigit = \asciiVal -> (asciiVal - 48)

# sumLine: List (Int a) -> (Int a)
sumLine = \lineAsciiDigits ->
    digitInts = List.map lineAsciiDigits asciiToDigit
    expect List.len digitInts > 0
    # TODO crash correctly here? https://www.roc-lang.org/tutorial#crashing-in-unreachable-branches
    firstDigit = Result.withDefault (List.get digitInts 0) 0
    lastDigit = Result.withDefault (List.last digitInts) 0
    # TODO: perf. Don't cast to string, come up with a bitshift trick
    numStr = Str.concat (Num.toStr firstDigit) (Num.toStr lastDigit)
    Result.withDefault (Str.toU32 numStr) 0


main =
    lines = List.walk sample { lineDigits: [], runningSum: 0u32 } \acc, byte ->
        if (isDigit byte) then
            byteU32 = Num.toU32 byte
            dbg T "Handling digit" acc byte
            { acc & lineDigits: List.append acc.lineDigits byteU32 }
        else if (isLineSep byte) then
            lineSum = sumLine acc.lineDigits
            dbg T "Finishing line" acc lineSum
            { lineDigits: [], runningSum: acc.runningSum + lineSum  }
        else
            dbg T "In else" acc byte
            acc

    finalSum = lines.runningSum + (sumLine lines.lineDigits)
    Num.toStr finalSum
        |> Stdout.line