app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
}

import cli.Stdout
import cli.Arg exposing [Arg]

import "input.txt" as input_str : Str

Tile : { x : I64, y : I64 }
Orientation : [Clockwise, CounterClockwise]
Direction : [UpRight, UpLeft, DownLeft, DownRight]
Rect : { max_x : I64, min_x : I64, max_y : I64, min_y : I64 }

main! : List Arg => Result {} _
main! = |_args|
    tiles = input_str |> parse_input()
    orientation = dertermine_orientation(tiles) |> Result.map_err(|e| Other(e))?

    tiles
    |> find_largest_inner_rect(orientation)
    |> Num.to_str()
    |> Stdout.line!()

find_largest_inner_rect : List Tile, Orientation -> I64
find_largest_inner_rect = |tiles, orientation|
    when tiles is
        [a, b] -> calculate_area(a, b)
        [first, second, .. as tail] ->
            lines = get_lines(tiles)

            tail
            |> List.append(first)
            |> List.walk(
                (0, first, second, tail |> List.append(first)),
                |(largest_area, prev, current, comparison_tiles), next|
                    directions = get_candidate_directions(prev, current, next, orientation)
                    area =
                        comparison_tiles
                        |> List.walk(
                            largest_area,
                            |walk_largest, walk_tile|
                                walk_area = calculate_area(walk_tile, current)
                                if walk_largest >= walk_area then
                                    walk_largest
                                else if !is_in_direction(walk_tile, current, directions) then
                                    walk_largest
                                else if is_obstructed(walk_tile, current, lines) then
                                    walk_largest
                                else
                                    walk_area,
                        )
                    (area, current, next, comparison_tiles |> List.drop_first(1)),
            )
            |> |(a, _, _, _)| a

        _ -> 0

get_candidate_directions : Tile, Tile, Tile, Orientation -> Set Direction
get_candidate_directions = |prev, current, next, orientation|
    when (get_direction(prev, current), get_direction(current, next), orientation) is
        ((Diff(Left), _), (_, Diff(Up)), Clockwise) | ((_, Diff(Down)), (Diff(Right), _), CounterClockwise) ->
            Set.single(UpRight)

        ((Diff(Left), _), (_, Diff(Up)), CounterClockwise) | ((_, Diff(Down)), (Diff(Right), _), Clockwise) ->
            Set.single(UpLeft) |> Set.insert(DownLeft) |> Set.insert(DownRight)

        ((Diff(Left), _), (_, Diff(Down)), CounterClockwise) | ((_, Diff(Up)), (Diff(Right), _), Clockwise) ->
            Set.single(DownRight)

        ((Diff(Left), _), (_, Diff(Down)), Clockwise) | ((_, Diff(Up)), (Diff(Right), _), CounterClockwise) ->
            Set.single(UpRight) |> Set.insert(UpLeft) |> Set.insert(DownLeft)

        ((Diff(Right), _), (_, Diff(Up)), CounterClockwise) | ((_, Diff(Down)), (Diff(Left), _), Clockwise) ->
            Set.single(UpLeft)

        ((Diff(Right), _), (_, Diff(Up)), Clockwise) | ((_, Diff(Down)), (Diff(Left), _), CounterClockwise) ->
            Set.single(DownLeft) |> Set.insert(DownRight) |> Set.insert(UpRight)

        ((Diff(Right), _), (_, Diff(Down)), Clockwise) | ((_, Diff(Up)), (Diff(Left), _), CounterClockwise) ->
            Set.single(DownLeft)

        ((Diff(Right), _), (_, Diff(Down)), CounterClockwise) | ((_, Diff(Up)), (Diff(Left), _), Clockwise) ->
            Set.single(DownRight) |> Set.insert(UpRight) |> Set.insert(UpLeft)

        _ -> Set.empty({})

is_in_direction : Tile, Tile, Set Direction -> Bool
is_in_direction = |tile, in_comparison_to, directions|
    when get_direction(in_comparison_to, tile) is
        (Same, Diff(Up)) -> Set.contains(directions, UpRight) or Set.contains(directions, UpLeft)
        (Same, Diff(Down)) -> Set.contains(directions, DownRight) or Set.contains(directions, DownLeft)
        (Diff(Left), Same) -> Set.contains(directions, UpLeft) or Set.contains(directions, DownLeft)
        (Diff(Right), Same) -> Set.contains(directions, UpRight) or Set.contains(directions, DownRight)
        (Diff(Left), Diff(Up)) -> Set.contains(directions, UpLeft)
        (Diff(Left), Diff(Down)) -> Set.contains(directions, DownLeft)
        (Diff(Right), Diff(Up)) -> Set.contains(directions, UpRight)
        (Diff(Right), Diff(Down)) -> Set.contains(directions, DownRight)
        _ -> Bool.false

get_direction : Tile, Tile -> ([Diff [Left, Right], Same], [Diff [Down, Up], Same])
get_direction = |source, target|
    x_direction = if source.x < target.x then Diff(Right) else if source.x > target.x then Diff(Left) else Same
    y_direction = if source.y < target.y then Diff(Up) else if source.y > target.y then Diff(Down) else Same
    (x_direction, y_direction)

is_obstructed : Tile, Tile, List (Tile, Tile) -> Bool
is_obstructed = |tile, in_comparison_to, lines|
    min_x = Num.min(tile.x, in_comparison_to.x)
    max_x = Num.max(tile.x, in_comparison_to.x)
    min_y = Num.min(tile.y, in_comparison_to.y)
    max_y = Num.max(tile.y, in_comparison_to.y)
    rect = { min_x, max_x, min_y, max_y }

    lines |> List.any(|line| line_obstructs(rect, line))

line_obstructs : Rect, (Tile, Tile) -> Bool
line_obstructs = |rect, (a,b)|
    if (a.x <= rect.min_x) != (b.x <= rect.min_x) and rect.min_y < a.y and a.y < rect.max_y then
        Bool.true
    else if (a.x >= rect.max_x) != (b.x >= rect.max_x) and rect.min_y < a.y and a.y < rect.max_y then
        Bool.true
    else if (a.y <= rect.min_y) != (b.y <= rect.min_y) and rect.min_x < a.x and a.x < rect.max_x then
        Bool.true
    else if (a.y >= rect.max_y) != (b.y >= rect.max_y) and rect.min_x < a.x and a.x < rect.max_x then
        Bool.true
    else
        Bool.false

get_lines : List Tile -> List (Tile, Tile)
get_lines = |tiles|
    when tiles is
        [] -> []
        [head, .. as tail] ->
            tail
            |> List.append(head)
            |> List.walk((head, []), |(prev, lines), tile| (tile, lines |> List.append((prev, tile))))
            |> |(_, lines)| lines

calculate_area : Tile, Tile -> I64
calculate_area = |a, b|
    (Num.abs(a.x - b.x) + 1) * (Num.abs(a.y - b.y) + 1)

dertermine_orientation : List Tile -> Result Orientation _
dertermine_orientation = |tiles|
    when tiles is
        [.., last] ->
            tiles
            |> List.walk(
                (Clockwise, last, { x: Num.max_i64, y: Num.max_i64 }),
                |(orientation, prev, min_corner), tile|
                    if tile.x < min_corner.x or (tile.x == min_corner.x and tile.y < min_corner.y) then
                        if tile.x == prev.x then
                            (CounterClockwise, tile, tile)
                        else
                            (Clockwise, tile, tile)
                    else
                        (orientation, tile, min_corner),
            )
            |> |(orientation, _, _)| Ok(orientation)

        _ -> Err("not enough tiles to determine orientation")

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

