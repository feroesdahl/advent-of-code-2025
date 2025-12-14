app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
}

import cli.Stdout
import cli.Arg exposing [Arg]

import "input.txt" as input_str : Str

Pos : { x : F64, y : F64, z : F64 }

main! : List Arg => Result {} _
main! = |_args|
    input_str
    |> parse_input()
    |> connect_circuits()
    |> Num.to_str()
    |> Stdout.line!()

connect_circuits : List Pos -> F64
connect_circuits = |positions|
    (dict, indices) = positions |> calculate_dist_dict()

    dict
    |> Dict.to_list()
    |> List.sort_with(
        |(_, (d1, _)), (_, (d2, _))|
            if d1 > d2 then
                GT
            else if d1 < d2 then
                LT
            else
                EQ,
    )
    |> List.map(|(pair, (_, p))| (pair, p))
    |> connect_loop(indices |> Set.map(|i| Set.single(i)))

connect_loop : List ((U64, U64), F64), Set (Set U64) -> F64
connect_loop = |pairs, circuits|
    (_, product) =
        pairs
        |> List.walk_until(
            (circuits, 0),
            |(state_circuits, _), ((x, y), p)|
                new_circuit = Set.empty({}) |> Set.insert(x) |> Set.insert(y)

                when state_circuits |> Set.to_list() |> List.keep_if(|s| s |> Set.intersection(new_circuit) |> Set.is_empty() |> Bool.not()) is
                    [.. as existing_circuits] ->
                        new_union = existing_circuits |> List.walk(new_circuit, |union, existing_circuit| union |> Set.union(existing_circuit))

                        new_state =
                            existing_circuits
                            |> List.walk(state_circuits, |cs, existing_circuit| cs |> Set.remove(existing_circuit))
                            |> Set.insert(new_union)

                        if Set.len(new_state) == 1 then Break((new_state, p)) else Continue((new_state, 0))

                    _ -> Continue((state_circuits |> Set.insert(new_circuit), 0)),
        )
    product

calculate_dist_dict : List Pos -> (Dict (U64, U64) (F64, F64), Set U64)
calculate_dist_dict = |positions|
    dict =
        positions
        |> List.walk_with_index(
            Dict.empty({}),
            |d, x_pos, x_index|
                positions
                |> List.drop_first(x_index + 1)
                |> List.walk_with_index(
                    d,
                    |state_dict, y_pos, y_index|
                        Dict.insert(state_dict, (x_index, y_index + x_index + 1), (dist(x_pos, y_pos), x_pos.x * y_pos.x)),
                ),
        )
    indices = List.range({ start: At 0, end: Before List.len(positions) }) |> Set.from_list()

    (dict, indices)

dist : Pos, Pos -> F64
dist = |a, b|
    x_term = Num.pow(a.x - b.x, 2)
    y_term = Num.pow(a.y - b.y, 2)
    z_term = Num.pow(a.z - b.z, 2)

    Num.sqrt(x_term + y_term + z_term)

parse_input : Str -> List Pos
parse_input = |input|
    input
    |> Str.trim_end()
    |> Str.split_on("\n")
    |> List.map(parse_row)

parse_row : Str -> Pos
parse_row = |row|
    when row |> Str.split_on(",") |> List.keep_oks(Str.to_f64) is
        [x, y, z] -> { x, y, z }
        _ -> { x: 0, y: 0, z: 0 }
