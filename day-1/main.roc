app "day-1-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "test.txt" as sample : Str,
    ]
    provides [main] to pf

lines = Str.split sample "\n"

expect
    numberOfLines = List.len lines
    numberOfLines == 4

# charToByte = \char -> Str.toUtf8 char |> List.get 0 |> Result.withDefault 0

# https://www.ascii-code.com/
zeroByte = '0'
expect zeroByte == 48
nineByte = '9'

isDigit = \charByte -> Bool.and (zeroByte <= charByte) (charByte <= nineByte)

expect (isDigit 'a') == Bool.false
expect (
    Str.toUtf8 "0123456789"
        |> List.all isDigit
) == Bool.true

byteToInt = \byte -> byte - zeroByte

expect ('1' |> byteToInt) == 1

## Extract the relevant digits as a list from the line.
getDigitsFromLine = \line ->
    digitsAcc = Str.walkUtf8 line { digits: [], lineBytes: [] } \acc, byte ->
        if isDigit byte then
            { acc& digits: List.append acc.digits (byteToInt byte) }
        else
            digitValue = when acc.lineBytes is
                ['z', 'e', 'r', 'o', .. ] -> ParsedDigitWithLen 0 4
                ['o', 'n', 'e', ..] -> ParsedDigitWithLen 1 3
                ['t', 'w', 'o', ..] -> ParsedDigitWithLen 2 3
                ['t', 'h', 'r', 'e', 'e', ..] -> ParsedDigitWithLen 3 5
                ['f', 'o', 'u', 'r', ..] -> ParsedDigitWithLen 4 4
                ['f', 'i', 'v', 'e', ..] -> ParsedDigitWithLen 5 4
                ['s', 'i', 'x', ..] -> ParsedDigitWithLen 6 3
                ['s', 'e', 'v', 'e', 'n', ..] -> ParsedDigitWithLen 7 5
                ['e', 'i', 'g', 'h', 't', ..] -> ParsedDigitWithLen 8 5
                ['n', 'i', 'n', 'e', ..] -> ParsedDigitWithLen 9 4
                _ -> NoneParsed

            when digitValue is
                ParsedDigitWithLen digit len ->
                    updatedLineBytes = List.dropFirst acc.lineBytes len
                    { lineBytes: updatedLineBytes, digits: List.append acc.digits digit }
                _ -> { acc& lineBytes: List.append acc.lineBytes byte }

    digitsAcc.digits

expect
    testLineDigit = getDigitsFromLine "123"
    testLineDigit == [1, 2, 3]

expect
    partTwoLine = getDigitsFromLine "1two3xfourx5"
    partTwoLine == [1, 2, 3, 4, 5]

getLineDigitNumber = \listOfDigits ->
    tensPlace = List.get listOfDigits 0 |> Result.withDefault 0
    onesPlace = List.last listOfDigits |> Result.withDefault tensPlace
    (tensPlace * 10) + onesPlace

main =
    fileSum = List.walk lines 0 \sum, line ->
        sum + (
            getDigitsFromLine line
                |> getLineDigitNumber
            )
    Num.toStr fileSum |> Stdout.line 