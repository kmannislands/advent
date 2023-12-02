app "day-2-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "input.txt" as sample : Str,
    ]
    provides [main] to pf

parseGame = \gameLine ->
    gameLinePieces = Str.split gameLine ": "

    gamePart = when List.get gameLinePieces 0 is
        Ok elem -> elem
        _ -> crash "Couldn't parse gameLine \(gameLine)"
    # draws = List.get gameLinePieces 0 |> Result.withDefault ""

    gameId = when Str.replaceFirst gamePart "Game " "" |> Str.toU32 is
        Ok gameIdInt -> gameIdInt
        _ -> crash "Failed to parse gameId \(gameLine)"

    { gameId: gameId }


main =
    lines = Str.split sample "\n"
    gameResults = List.map lines \line -> parseGame line
    dbg gameResults
    Stdout.line "done"