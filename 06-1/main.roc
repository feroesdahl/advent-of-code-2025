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
    |> calculate()
    |> Num.to_str()
    |> Stdout.line!()

calculate : { number_rows : List List U64, operators : List Str } -> U64
calculate = |input|
    walk =
        input.number_rows
        |> List.walk(
            None,
            |acc_row, next_row|
                when acc_row is
                    None -> Some(next_row)
                    Some(row) -> Some(apply_operator(row, next_row, input.operators)),
        )

    when walk is
        None -> 0
        Some(row) -> List.sum(row)

apply_operator : List U64, List U64, List Str -> List U64
apply_operator = |number_row_a, number_row_b, operators|
    List.map3(number_row_a, number_row_b, operators, operate)

operate : U64, U64, Str -> U64
operate = |a, b, operator|
    when operator is
        "*" -> a * b
        "+" -> a + b
        _ -> 0

parse_input : Str -> { number_rows : List List U64, operators : List Str }
parse_input = |input|
    rows =
        input
        |> Str.trim()
        |> Str.split_on("\n")
        |> List.map(|x| x |> Str.split_on(" "))
        |> List.map(|x| x |> List.keep_if(|s| !Str.is_empty(s)))

    split = List.split_at(rows, List.len(rows) - 1)
    {
        number_rows: split.before |> List.map(|x| x |> List.keep_oks(Str.to_u64)),
        operators: split.others |> List.first() |> Result.with_default([]),
    }
