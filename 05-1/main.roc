app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.3.1/1PCTQ0tzSijxfhxDg1k_yPtfOXiAk3j283b8EWGusVc.tar.br",
}

import cli.Stdout
import cli.Arg exposing [Arg]

import "input.txt" as input_str : Str

main! : List Arg => Result {} _
main! = |_args|
    input_str
    |> parse_input()
    |> count_fresh_available()
    |> Num.to_str()
    |> Stdout.line!()

count_fresh_available : { fresh : List { start : U64, end : U64 }, available : List U64 } -> U64
count_fresh_available = |input|
    List.walk(
        input.available,
        0,
        |count, next|
            if is_fresh(next, input.fresh) then
                count + 1
            else
                count,
    )

is_fresh : U64, List { start : U64, end : U64 } -> Bool
is_fresh = |ingredient, fresh|
    fresh |> List.any(|x| x.start <= ingredient and ingredient <= x.end)

parse_input : Str -> { fresh : List { start : U64, end : U64 }, available : List U64 }
parse_input = |input|
    split = input |> Str.trim() |> Str.split_on("\n") |> List.split_on("")
    fresh = split |> List.first() |> Result.with_default([]) |> List.keep_oks(extract_range)
    available = split |> List.last() |> Result.with_default([]) |> List.keep_oks(Str.to_u64)
    { fresh: fresh, available: available }

extract_range : Str -> Result { start : U64, end : U64 } _
extract_range = |range_str|
    split = Str.split_first(range_str, "-")?
    start = Str.to_u64(split.before)?
    end = Str.to_u64(split.after)?
    Ok({ start: start, end: end })

