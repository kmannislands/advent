app "day-1-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "input.txt" as sample : List U8,
    ]
    provides [main] to pf

strJoin: List Str, Str -> Str
strJoin = \list, joinStr ->
    List.walk list "" \acc, str ->
        Str.concat acc joinStr
        |> Str.concat str

main =
    strsList = List.map sample \(bytes) -> Num.toStr bytes

    Stdout.line (strJoin strsList ", ")

