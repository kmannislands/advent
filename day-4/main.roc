app "day-2-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "input.txt" as sample : Str,
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

drawPoints : Nat -> U32
drawPoints = \countWinningNumbers ->
    if countWinningNumbers == 0 then
        0
    else
        # > The first match makes the card worth one point and each match after the first doubles the point value
        # > of that card.
        pointPower = Num.toU32 countWinningNumbers |> Num.subChecked 1 |> Result.withDefault 0
        points = Num.powInt 2 pointPower
        points

winningNumbers : Draw -> Nat
winningNumbers = \{ winningNums, numbers } ->
    Set.intersection winningNums numbers |> Set.len

# > ...scratchcards only cause you to win more scratchcards equal to the number of winning numbers you have.
# > Specifically, you win copies of the scratchcards below the winning card equal to the number of matches.
# > So, if card 10 were to have 5 matching numbers, you would win one copy each of cards 11, 12, 13, 14, and 15.
finalNumberOftickets : List Nat -> U32
finalNumberOftickets = \wins ->
    initialState : WalkerState
    initialState = { nextMultiples: (List.repeat 1 233), countTickets: 0 }
    List.walk wins initialState reducer |> .countTickets

WalkerState : {
    nextMultiples: List Nat,
    countTickets: U32
}


reducer : WalkerState, Nat -> WalkerState
reducer = \state, win ->
    copiesOfCurrentCard = List.get state.nextMultiples 0 |> Result.withDefault 1
    nextMultiples = List.dropFirst state.nextMultiples 1
        |> List.mapWithIndex \copiesOfNextCard, i ->
            if i < win then
                (copiesOfNextCard + copiesOfCurrentCard)
            else
                win

    { nextMultiples, countTickets: state.countTickets + (Num.toU32 copiesOfCurrentCard) }

main =
    draws = scratchOffDraws sample
    drawWins : List Nat
    drawWins = List.map draws winningNumbers
    totalPoints = List.walk drawWins 0 \runningSum, wins -> runningSum + (drawPoints wins)
    totalTickets = finalNumberOftickets drawWins
    Stdout.line "Total points: \(Num.toStr totalPoints). Total tickets at halt: \(Num.toStr totalTickets)"