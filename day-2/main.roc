app "day-2-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "input.txt" as sample : Str,
    ]
    provides [main] to pf

parseGameId = \gamePart ->
    when Str.replaceFirst gamePart "Game " "" |> Str.toU32 is
        Ok gameIdInt -> gameIdInt
        _ -> crash "Failed to parse gameId from partial line '\(gamePart)'"

# Draw : {
#     red: Num U8,
#     blue: Num U8,
#     green: Num U8,
# }
# parseDraws: Str -> List Draw
parseDraws = \drawsPart ->
    drawStrs = Str.split drawsPart "; "
    draws = List.map drawStrs \_drawStr ->
        { red: 0, green: 0, blue: 0 }
    draws

        
    
parseGame = \gameLine ->
    gameLinePieces = Str.split gameLine ": "
    when gameLinePieces is
        [gamePart, drawsPart] -> { gameId: parseGameId gamePart, draws: parseDraws drawsPart }
        _ -> crash "Couldn't parse gameLine \(gameLine)"

main =
    lines = Str.split sample "\n"
    gameResults = List.map lines \line -> parseGame line
    dbg gameResults
    Stdout.line "done"