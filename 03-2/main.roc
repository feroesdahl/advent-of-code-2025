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
  |> Str.split_on("\n")
  |> List.map(get_bank)
  |> List.map(get_largest_joltage)
  |> List.sum()
  |> Num.to_str()
  |> Stdout.line!()

get_bank : Str -> List U8
get_bank = |str|
  Ascii.from_str(str)
  |> Result.with_default([])
  |> List.map(|c| Ascii.to_str([c]) |> Str.to_u8() |> Result.with_default(0))

get_largest_joltage : List U8 -> U128
get_largest_joltage = |bank|
  get_largest_joltage_loop(12, bank, 0) |> Result.with_default(0)

get_largest_joltage_loop : U64, List U8, U128 -> Result U128 _
get_largest_joltage_loop = |size, bank, acc|
  when size is
    0 -> Ok(acc)
    n ->
      split = List.split_at(bank, List.len(bank) - n + 1)
      max = List.max(split.before)? 
      tail = List.split_first(bank, max) |> Result.map_ok(|x| x.after)?  
      get_largest_joltage_loop(n - 1, tail, acc + Num.to_u128(max) * Num.pow_int(10, Num.to_u128(n) - 1))
