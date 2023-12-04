app "day-2-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "test.txt" as sample : Str,
    ]
    provides [main] to pf

# Take-aways from day 2:
# - Understand the type system better. Complex code gets really out of hand without declarations
# - Keep up the testing, that was good.

main =
    oneSet = Set.empty {} |> Set.insert [0, 1]
    hasVector = Set.contains oneSet [0, 1]
    strToPrint = if (hasVector == Bool.true) then "Worked" else "Didn't work"

    Stdout.line strToPrint