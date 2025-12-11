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
    |> Str.split_on("\n")
    |> List.map(get_bank)
    |> List.map(get_largest_joltage)
    |> List.map(Num.to_u64)
    |> List.sum()
    |> Num.to_str()
    |> Stdout.line!()

get_bank : Str -> List U8
get_bank = |str|
    Ascii.from_str(str)
    |> Result.with_default([])
    |> List.map(|c| Ascii.to_str([c]) |> Str.to_u8() |> Result.with_default(0))

get_largest_joltage : List U8 -> U8
get_largest_joltage = |bank|
    List.walk_backwards(bank, 0, get_largest_joltage_step)

get_largest_joltage_step : U8, U8 -> U8
get_largest_joltage_step = |best, next|
    when best is
        0 -> next
        n -> Num.max(best, (next * 10) + Num.max(n // 10, n % 10))
