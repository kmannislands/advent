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
    winningNums: List U32,
    numbers: List U32
}

scratchOffDraws : Str -> List Draw
scratchOffDraws = \drawsStr ->
    drawLines = Str.split drawsStr "\n"
    List.map drawLines drawLine
        

drawLine: Str -> Draw
drawLine = \drawStr ->
    when Str.split drawStr " | " is
        [winningNumbersStr, drawnNumbersStr] -> {
            winningNums: strToNumbers winningNumbersStr,
            numbers: strToNumbers drawnNumbersStr
        }
        _ -> crash "Malformed draw result"

strToNumbers : Str -> List U32
strToNumbers = \numbersStr ->
    Str.trim numbersStr |> Str.split " "
        |> List.keepOks \numStr ->
            Str.trim numStr |> Str.toU32

main =
    dbg scratchOffDraws sample
    Stdout.line sample