app [main!] {
  cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
  ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.3.1/1PCTQ0tzSijxfhxDg1k_yPtfOXiAk3j283b8EWGusVc.tar.br"
}

import cli.Stdout
import cli.Arg exposing [Arg]
import ascii.Ascii

import "input.txt" as input_str : Str

main! : List Arg => Result {} _
main! = |_args|
  input_str
  |> Str.trim()
  |> Str.split_on(",")
  |> List.map(extract_invalid_ids)
  |> List.join()
  |> List.sum()
  |> Num.to_str()
  |> Stdout.line!()

extract_invalid_ids : Str -> List U64
extract_invalid_ids = |str|
  str
  |> extract_ids_from_range
  |> Result.with_default([])
  |> List.keep_if(id_is_invalid)

extract_ids_from_range : Str -> Result (List U64) _
extract_ids_from_range = |id|
  split = Str.split_first(id, "-")?
  start = Str.to_u64(split.before)?
  end = Str.to_u64(split.after)?
  Ok(List.range({ start: At start, end: At end }))

id_is_invalid : U64 -> Bool
id_is_invalid = |id|
  ascii_id = Num.to_str(id) |> Ascii.from_str() |> Result.with_default([])
  len = List.len(ascii_id)
  List.range({ start: After 0, end: At (len // 2)})
  |> List.keep_if(|n| (len % n == 0))
  |> List.map(|n| split_id(ascii_id, n))
  |> List.any(all_equal)

split_id : Ascii.Ascii, U64 -> List Ascii.Ascii
split_id = |list, n|
  when list is
    [] -> []
    l ->
      split = List.split_at(l, n)
      List.prepend(split_id(split.others, n), split.before)

all_equal : List Ascii.Ascii -> Bool
all_equal = |list|
  list_str = List.map(list, Ascii.to_str)
  List.all(list_str, |a| a == List.first(list_str) |> Result.with_default(""))

