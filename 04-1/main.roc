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
    |> get_grid()
    |> get_accesible_roll_count()
    |> Num.to_str()
    |> Stdout.line!()

get_accesible_roll_count : Dict I64 (Set I64) -> U64
get_accesible_roll_count = |grid|
    Dict.walk(
        grid,
        0,
        |state, row_index, row_set|
            Set.walk(
                row_set,
                state,
                |row_state, column_index|
                    if is_accesible(row_index, column_index, grid) then row_state + 1 else row_state,
            ),
    )

is_accesible : I64, I64, Dict I64 (Set I64) -> Bool
is_accesible = |row_index, column_index, grid|
    get_adjacent_roll_count(row_index, column_index, grid) < 4

get_adjacent_roll_count : I64, I64, Dict I64 (Set I64) -> U8
get_adjacent_roll_count = |row_index, column_index, grid|
    get_adjacent_coordinates(row_index, column_index)
    |> List.keep_if(|(r, c)| grid_contains(r, c, grid))
    |> List.len()
    |> Num.to_u8()

grid_contains : I64, I64, Dict I64 (Set I64) -> Bool
grid_contains = |row_index, column_index, grid|
    when Dict.get(grid, row_index) is
        Ok(s) -> Set.contains(s, column_index)
        _ -> Bool.false

get_adjacent_coordinates : I64, I64 -> List (I64, I64)
get_adjacent_coordinates = |row_index, column_index|
    above = [(row_index - 1, column_index - 1), (row_index - 1, column_index), (row_index - 1, column_index + 1)]
    same_row = [(row_index, column_index - 1), (row_index, column_index + 1)]
    below = [(row_index + 1, column_index - 1), (row_index + 1, column_index), (row_index + 1, column_index + 1)]

    above
    |> List.concat(same_row)
    |> List.concat(below)

get_grid : Str -> Dict I64 (Set I64)
get_grid = |grid|
    grid
    |> Str.trim()
    |> Str.split_on("\n")
    |> List.map(Ascii.from_str)
    |> List.map(|s| Result.with_default(s, []))
    |> walk_rows()

walk_rows : List Ascii.Ascii -> Dict I64 (Set I64)
walk_rows = |rows|
    List.walk_with_index(
        rows,
        Dict.empty({}),
        |row_state, row, row_index|
            row_state
            |> Dict.insert(
                Num.to_i64(row_index),
                walk_row(row),
            ),
    )

walk_row : Ascii.Ascii -> Set I64
walk_row = |row|
    List.walk_with_index(
        row,
        Set.empty({}),
        |state, char, index|
            when Ascii.to_str([char]) is
                "@" -> state |> Set.insert(Num.to_i64(index))
                _ -> state,
    )
