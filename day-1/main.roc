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

charToByte = \char -> Str.toUtf8 char |> List.get 0 |> Result.withDefault 0

# https://www.ascii-code.com/
zeroByte = charToByte "0"
expect zeroByte == 48
nineByte = charToByte "9"

isDigit = \charByte -> Bool.and (zeroByte <= charByte) (charByte <= nineByte)

expect (charToByte "a" |> isDigit) == Bool.false
expect (
    Str.toUtf8 "0123456789"
        |> List.all isDigit
) == Bool.true

byteToInt = \byte -> byte - zeroByte

expect (charToByte "1" |> byteToInt) == 1

## Extract the relevant digits as a list from the line.
getDigitsFromLine = \line ->
    digitsAcc = Str.walkUtf8 line { digits: [], lineBytes: [] } \acc, byte ->
        if isDigit byte then
            { acc& digits: List.append acc.digits (byteToInt byte) }
        else
            acc

    digitsAcc.digits

expect
    testLineDigit = getDigitsFromLine "123"
    testLineDigit == [1, 2, 3]

main =
    Stdout.line sample