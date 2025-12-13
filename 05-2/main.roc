app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
}

import cli.Stdout
import cli.Arg exposing [Arg]

import "input.txt" as input_str : Str

main! : List Arg => Result {} _
main! = |_args|
    input_str
    |> parse_input()
    |> count_fresh()
    |> Num.to_str()
    |> Stdout.line!()

count_fresh : List { start : U64, end : U64 } -> U64
count_fresh = |input|
    walk =
        input
        |> List.sort_with(range_comparer)
        |> List.walk({ acc: 0, cur_range: None }, fresh_count_walk)

    when walk.cur_range is
        None -> walk.acc
        Ok(range) -> walk.acc + get_range_count(range)

fresh_count_walk :
    { acc : U64, cur_range : [Ok { start : U64, end : U64 }, None] },
    { start : U64, end : U64 }
    -> { acc : U64, cur_range : [Ok { start : U64, end : U64 }, None] }
fresh_count_walk = |state, next_range|
    when state.cur_range is
        None -> { acc: 0, cur_range: Ok(next_range) }
        Ok(range) if next_range.start > range.end -> { acc: state.acc + get_range_count(range), cur_range: Ok(next_range) }
        Ok(range) -> { acc: state.acc, cur_range: Ok({ start: range.start, end: Num.max(next_range.end, range.end) }) }

get_range_count : { start : U64, end : U64 } -> U64
get_range_count = |range|
    range.end - range.start + 1

range_comparer : { start : U64, end : U64 }, { start : U64, end : U64 } -> [LT, EQ, GT]
range_comparer = |a, b|
    if a.start < b.start then
        LT
    else if a.start > b.start then
        GT
    else if a.end < b.end then
        LT
    else if a.end > b.end then
        GT
    else
        EQ

parse_input : Str -> List { start : U64, end : U64 }
parse_input = |input|
    input
    |> Str.trim()
    |> Str.split_on("\n")
    |> List.split_on("")
    |> List.first()
    |> Result.with_default([])
    |> List.keep_oks(extract_range)

extract_range : Str -> Result { start : U64, end : U64 } _
extract_range = |range_str|
    split = Str.split_first(range_str, "-")?
    start = Str.to_u64(split.before)?
    end = Str.to_u64(split.after)?
    Ok({ start: start, end: end })

