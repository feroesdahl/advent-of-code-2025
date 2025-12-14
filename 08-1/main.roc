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
    |> calculate_circuits(1000)
    |> multiply_circuits(3)
    |> Num.to_str()
    |> Stdout.line!()

multiply_circuits : Set (Set U64), U64 -> U64
multiply_circuits = |circuits, count|
    circuits
    |> Set.to_list()
    |> List.map(Set.len)
    |> List.sort_desc()
    |> List.take_first(count)
    |> List.product()

calculate_circuits : List Pos, U64 -> Set (Set U64)
calculate_circuits = |positions, cable_count|
    positions
    |> calculate_dist_dict()
    |> Dict.to_list()
    |> List.sort_with(
        |(_, d1), (_, d2)|
            if d1 > d2 then
                GT
            else if d1 < d2 then
                LT
            else
                EQ,
    )
    |> List.take_first(cable_count)
    |> List.walk(
        Set.empty({}),
        |circuits, ((x_index, y_index), _)|
            new_circuit = Set.empty({}) |> Set.insert(x_index) |> Set.insert(y_index)

            when circuits |> Set.to_list() |> List.keep_if(|s| s |> Set.intersection(new_circuit) |> Set.is_empty() |> Bool.not()) is
                [.. as existing_circuits] ->
                    new_union = existing_circuits |> List.walk(new_circuit, |union, existing_circuit| union |> Set.union(existing_circuit))

                    existing_circuits
                    |> List.walk(circuits, |state_circuits, existing_circuit| state_circuits |> Set.remove(existing_circuit))
                    |> Set.insert(new_union)

                _ -> circuits |> Set.insert(new_circuit)
        )

calculate_dist_dict : List Pos -> Dict (U64, U64) F64
calculate_dist_dict = |positions|
    positions
    |> List.walk_with_index(
        Dict.empty({}),
        |dict, x_pos, x_index|
            positions
            |> List.drop_first(x_index + 1)
            |> List.walk_with_index(
                dict,
                |state_dict, y_pos, y_index|
                    Dict.insert(state_dict, (x_index, y_index + x_index + 1), dist(x_pos, y_pos)),
            ),
    )

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
