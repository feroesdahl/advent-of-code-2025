app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.3.1/1PCTQ0tzSijxfhxDg1k_yPtfOXiAk3j283b8EWGusVc.tar.br",
}

import cli.Stdout
import cli.Arg exposing [Arg]
import ascii.Ascii

import "input.txt" as input_str : Str

main! : List Arg => Result {} _
main! = |_args|
    input_str
    |> Str.trim()
    |> get_ids()
    |> List.keep_if(id_is_invalid)
    |> List.sum()
    |> Num.to_str()
    |> Stdout.line!()

get_ids : Str -> List U64
get_ids = |input|
    Str.split_on(input, ",")
    |> List.map(|x| extract_ids_from_range(x) |> Result.with_default([]))
    |> List.join()

extract_ids_from_range : Str -> Result (List U64) _
extract_ids_from_range = |id|
    split = Str.split_first(id, "-")?
    start = Str.to_u64(split.before)?
    end = Str.to_u64(split.after)?
    Ok(List.range({ start: At start, end: At end }))

id_is_invalid : U64 -> Bool
id_is_invalid = |id|
    (a, b) = Num.to_str(id) |> Ascii.from_str() |> Result.map_ok(split_list_in_half) |> Result.with_default(([], []))
    Str.caseless_ascii_equals(Ascii.to_str(a), Ascii.to_str(b))

split_list_in_half : Ascii.Ascii -> (Ascii.Ascii, Ascii.Ascii)
split_list_in_half = |l|
    split = List.split_at(l, (List.len(l) // 2))
    (split.before, split.others)

