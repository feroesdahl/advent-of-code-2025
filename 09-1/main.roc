app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
}

import cli.Stdout
import cli.Arg exposing [Arg]

import "input.txt" as input_str : Str

Tile : { x : I64, y : I64 }

main! : List Arg => Result {} _
main! = |_args|
    input_str
    |> parse_input()
    |> find_largest_rect_area()
    |> Num.to_str()
    |> Stdout.line!()

find_largest_rect_area : List Tile -> I64
find_largest_rect_area = |tiles|
    loop = |tail, max|
        when tail is
            [cur, .. as next] ->
                cur_max = next |> List.walk(max, |state_max, tile| Num.max(calculate_area(cur, tile), state_max))
                loop(next, cur_max)

            [] -> max

    loop(tiles, 0)

calculate_area : Tile, Tile -> I64
calculate_area = |a, b|
    (Num.abs(a.x - b.x) + 1) * (Num.abs(a.y - b.y) + 1)

parse_input : Str -> List Tile
parse_input = |input|
    input
    |> Str.trim_end()
    |> Str.split_on("\n")
    |> List.map(
        |row|
            coordinate_list = row |> Str.split_on(",") |> List.keep_oks(Str.to_i64)
            when coordinate_list is
                [x, y] -> { x, y }
                _ -> { x: 0, y: 0 },
    )
