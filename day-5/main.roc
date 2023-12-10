app "day-2-solution"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "test.txt" as sample : Str,
    ]
    provides [main] to pf

## Utility to split lists
popFirst : List a -> Result { first: a, rest: List a } [OutOfBounds]
popFirst = \list ->
    List.get list 0
        |> Result.map \first -> { first, rest: List.dropFirst list 1 }

expect
    popFirst [1, 2, 3] == Ok { first: 1, rest: [2, 3] }

expect
    popFirst [] == Err OutOfBounds


## Parse a single line of the lookup
LookupRange : {
    dest: U32,
    src: U32,
    len: U32,
}

lookupRow : Str -> LookupRange
lookupRow = \lookupLineStr ->
    parts = Str.split lookupLineStr " "

    when parts is
        # Non-ideal, would swallow U32 errors
        [dest, src, len] -> {
            dest: Str.toU32 dest |> Result.withDefault 0,
            src: Str.toU32 src |> Result.withDefault 0,
            len: Str.toU32 len |> Result.withDefault 0,
        }
        _ -> crash "Lookup row wasn't in the expected format \(lookupLineStr)"

expect
    lookupRow "50 98 2" == { dest: 50, src: 98, len: 2 }


# Ie seed -> soil
Lookup : {
    from: Str,
    to: Str,
    ranges: List LookupRange
}

## Parse a single lookup dictionary
lookup : Str -> Lookup
lookup = \lookupStr ->
    lines = Str.split lookupStr "\n"
    { first, rest } = when popFirst lines is
        Ok n -> n
        _ -> crash "didn't get at least one line from list"
    fromToParts = Str.split first "-to-"

    when fromToParts is
        [from, toPlusMap] ->
            to = Str.replaceLast toPlusMap " map:" ""
            {
                from: from,
                to,
                ranges: List.map rest lookupRow
            }
        _ -> crash "From to wasn't in the expected shape \(first)"

expect
    parsed = lookup "seed-to-soil map:\n50 98 2\n52 50 48"
    parsed == {
        from: "seed",
        to: "soil",
        ranges: [
            { dest: 50, src: 98, len: 2 },
            { dest: 52, src: 50, len: 48 }
        ]
    }

performRowLookup : LookupRange -> (U32 -> Result U32 [NotMapped])
performRowLookup = \{ src, dest, len } ->
    rangeDiff = src - dest
    \srcVal ->
        isInSrcRange = (srcVal >= src) && srcVal <= (src + (len - 1))
        if isInSrcRange then
            # return the mapped value
            Ok (srcVal - rangeDiff)
        else
            Err NotMapped

expect
    lookupFn = performRowLookup { dest: 50, src: 98, len: 2 }
    mappedVal = lookupFn 99
    mappedVal == Ok 51

expect
    lookupFn = performRowLookup { dest: 50, src: 98, len: 2 }
    lookupFn 100 == Err NotMapped



main =
    parsed = lookup "seed-to-soil map:\n50 98 2\n52 50 48"
    dbg parsed
    Stdout.line sample