app "day-2-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "test.txt" as sample : Str,
    ]
    provides [main] to pf

# LookupRange : {
#     destRangeStart: U32,
#     srcRangeStart: U32,
#     rangeLength: U32,
# }

# Ie seed -> soil
Lookup : {
    from: Str,
    to: Str,
    # ranges: List LookupRange
}

popFirst : List a -> Result { first: a, rest: List a } [OutOfBounds]
popFirst = \list ->
    List.get list 0
        |> Result.map \first -> { first, rest: List.dropFirst list 1 }

expect
    popFirst [1, 2, 3] == Ok { first: 1, rest: [2, 3] }

expect
    popFirst [] == Err OutOfBounds

lookup : Str -> Lookup
lookup = \lookupStr ->
    lines = Str.split lookupStr "\n"
    { first } = when popFirst lines is
        Ok n -> n
        _ -> crash "didn't get at least one line from list"
    fromToParts = Str.split first "-to-"

    when fromToParts is
        [from, toPlusMap] -> { from: from, to: Str.replaceLast toPlusMap " map:" "" }
        _ -> crash "From to wasn't in the expected shape \(first)"

expect
    parsed = lookup "seed-to-soil map:\n"
    parsed == { from: "seed", to: "soil" }

main =
    parsed = lookup "seed-to-soil map:\n50 98 2\n52 50 48"
    dbg parsed
    Stdout.line sample