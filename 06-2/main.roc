app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.3.1/1PCTQ0tzSijxfhxDg1k_yPtfOXiAk3j283b8EWGusVc.tar.br",
}

import cli.Stdout
import cli.Arg exposing [Arg]
import ascii.Ascii
import ascii.Char

import "input.txt" as input_str : Str

main! : List Arg => Result {} _
main! = |_args|
    input_str
    |> parse_input()
    |> calculate()
    |> Num.to_str()
    |> Stdout.line!()

calculate : { number_columns : List (List U64), operators : List Str } -> U64
calculate = |input|
    List.map2(input.number_columns, input.operators, apply_operator) |> List.sum()

apply_operator : List U64, Str -> U64
apply_operator = |numbers, operator|
    when operator is
        "*" -> List.product(numbers)
        "+" -> List.sum(numbers)
        _ -> 0

parse_input : Str -> { number_columns : List (List U64), operators : List Str }
parse_input = |input|
    rows =
        input
        |> Str.trim_end()
        |> Str.split_on("\n")

    operators =
        List.last(rows)
        |> Result.with_default("")
        |> Str.split_on(" ")
        |> List.keep_if(|s| !Str.is_empty(s))

    (first_chunks, last_chunk) =
        rows
        |> List.keep_oks(Ascii.from_str)
        |> transpose()
        |> Result.with_default([])
        |> List.walk(([], []), walk_chunk)
    chunks = List.append(first_chunks, last_chunk) |> List.drop_first(1)

    number_columns =
        chunks
        |> List.map(
            |chunk_row|
                chunk_row
                |> List.map(Ascii.to_str)
                |> List.map(Str.trim)
                |> List.keep_if(|s| !Str.is_empty(s))
                |> List.keep_oks(Str.to_u64)
        )

    { number_columns: number_columns, operators: operators }

walk_chunk : (List List Ascii.Ascii, List Ascii.Ascii), Ascii.Ascii -> (List List Ascii.Ascii, List Ascii.Ascii)
walk_chunk = |(chunks, cur_chunk), column|
    operator = List.last(column) |> Result.map_ok(|c| Ascii.to_str([c]))
    chunk_column = List.drop_last(column, 1)

    when operator is
        Ok(" ") -> (chunks, List.append(cur_chunk, chunk_column))
        _ -> (List.append(chunks, cur_chunk), [chunk_column])

transpose : List Ascii.Ascii -> Result (List Ascii.Ascii) _
transpose = |rows|
    len = rows |> List.map(List.len) |> List.max() |> Result.with_default(0)
    space = Char.from_ascii_byte(32)?

    output =
        rows
        |> List.walk(
            List.repeat([], len),
            |columns, row|
                padded_row = List.concat(row, List.repeat(space, len - List.len(row)))
                List.map2(columns, padded_row, |column, entry| List.append(column, entry)),
        )
    Ok(output)
