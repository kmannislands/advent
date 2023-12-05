app "day-2-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "test.txt" as sample : Str,
    ]
    provides [main] to pf

# Day 4, little tired today let's try just KISS
# I found that writing roc without many tests was more fun and actually easier. I think the combination of the
# type system's strictness and lack of editor support plus the fact that `dbg` statements don't seem to work with
# `roc test` made the loop unpleasant. Let's go without tests today.

Draw : {
    winningNums: Set U32,
    numbers: Set U32
}

scratchOffDraws : Str -> List Draw
scratchOffDraws = \drawsStr ->
    drawLines = Str.split drawsStr "\n"
    List.map drawLines drawLine
        

drawLine: Str -> Draw
drawLine = \drawStr ->
    when Str.split drawStr " | " is
        [winningNumbersStr, drawnNumbersStr] -> {
            winningNums: strToNumbers winningNumbersStr |> Set.fromList,
            numbers: strToNumbers drawnNumbersStr |> Set.fromList
        }
        _ -> crash "Malformed draw result"

## Parse a space separated list of numbers to U32s
strToNumbers : Str -> List U32
strToNumbers = \numbersStr ->
    Str.trim numbersStr |> Str.split " "
        |> List.keepOks \numStr ->
            Str.trim numStr |> Str.toU32

drawPoints : Draw -> U32
drawPoints = \{ winningNums, numbers } ->
    countWinningNumbers = Set.intersection winningNums numbers |> Set.len |> Num.toU32
    if countWinningNumbers == 0 then
        0
    else
        # > The first match makes the card worth one point and each match after the first doubles the point value
        # > of that card.
        pointPower = Num.subChecked countWinningNumbers 1 |> Result.withDefault 0
        points = Num.powInt 2 pointPower
        points

main =
    draws = scratchOffDraws sample
    totalPoints = List.walk draws 0 \runningSum, draw -> runningSum + (drawPoints draw)
    Stdout.line "Total points: \(Num.toStr totalPoints)"