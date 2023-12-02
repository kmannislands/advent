app "day-1-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "input.txt" as sample : Str,
    ]
    provides [main] to pf

# Convenience aliases the numeric byte representation of the smallest and largest digits
zeroByte = '0'
nineByte = '9'

expect zeroByte == 48

## Given as ASCII byte, return whether its a valid digit
isDigit = \charByte -> Bool.and (zeroByte <= charByte) (charByte <= nineByte)

expect (isDigit 'a') == Bool.false
expect (
    Str.toUtf8 "0123456789"
        |> List.all isDigit
) == Bool.true

## Convert the utf8/ascii byte for a digit to the U32 integer
## see: https://www.ascii-code.com/
## NB, at least U32 must be used for advent day 1 challenge else you'll get an overflow error
byteToInt = \byte -> Num.toU32 (byte - zeroByte)

expect ('1' |> byteToInt) == 1

## Given a (partial) string as bytes, parse out any digits that are spelled out at the end of the string,
## returning ParsedDigitWithLen with the numeric value of the digit if successful, NoneParsed otherwise
parseDigitFromStrEnd = \bytes ->
    when bytes is
        [.., 'z', 'e', 'r', 'o'] -> ParsedDigitWithLen 0
        [.., 'o', 'n', 'e'] -> ParsedDigitWithLen 1
        [.., 't', 'w', 'o'] -> ParsedDigitWithLen 2
        [.., 't', 'h', 'r', 'e', 'e'] -> ParsedDigitWithLen 3
        [.., 'f', 'o', 'u', 'r'] -> ParsedDigitWithLen 4
        [.., 'f', 'i', 'v', 'e'] -> ParsedDigitWithLen 5
        [.., 's', 'i', 'x'] -> ParsedDigitWithLen 6
        [.., 's', 'e', 'v', 'e', 'n'] -> ParsedDigitWithLen 7
        [.., 'e', 'i', 'g', 'h', 't'] -> ParsedDigitWithLen 8
        [.., 'n', 'i', 'n', 'e'] -> ParsedDigitWithLen 9
        _ -> NoneParsed

## Number of characters at the end of the spelling of one letter that could be used in the start of the next letter.
## ex: eighthree oneight
##         ^       ^
overlapChars = 1

## Extract the relevant digits as a list from the line.
getDigitsFromLine = \line ->
    digitsAcc = Str.walkUtf8 line { digits: [], lineBytes: [] } \acc, byte ->
        if isDigit byte then
            { lineBytes: [], digits: List.append acc.digits (byteToInt byte) }
        else
            currentLineBytes = List.append acc.lineBytes byte            
            when parseDigitFromStrEnd currentLineBytes is
                ParsedDigitWithLen digit ->
                    updatedLineBytes = List.dropFirst currentLineBytes overlapChars
                    { lineBytes: updatedLineBytes, digits: List.append acc.digits digit }
                _ -> { acc& lineBytes: currentLineBytes }

    digitsAcc.digits

expect
    testLineDigit = getDigitsFromLine "123"
    testLineDigit == [1, 2, 3]

expect
    partTwoLine = getDigitsFromLine "9sixsevenz3"
    partTwoLine == [9, 6, 7, 3]

expect
    partTwoLine = getDigitsFromLine "1two3xfourx5"
    partTwoLine == [1, 2, 3, 4, 5]

expect
    # This test is tricky because "one" and "eight" overlap, sharing the "e"
    partTwoLine = getDigitsFromLine "zoneight234"
    partTwoLine == [1, 8, 2, 3, 4]

## Combine digits for a line as-specified, using the first and last digit to form a 2-digit decimal number
## Note that if there is only one digit, it should be used in both places
getLineDigitNumber = \listOfDigits ->
    tensPlace = List.get listOfDigits 0 |> Result.withDefault 0
    onesPlace = List.last listOfDigits |> Result.withDefault tensPlace
    (tensPlace * 10) + onesPlace

expect
    getLineDigitNumber [2] == 22

expect
    getLineDigitNumber [9, 2, 3] == 93

main =
    lines = Str.split sample "\n"
    fileSum = List.walk lines 0 \sum, line ->
        sum + (
            getDigitsFromLine line |> getLineDigitNumber
        )
    Num.toStr fileSum |> Stdout.line 