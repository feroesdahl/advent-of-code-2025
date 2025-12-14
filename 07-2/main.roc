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
    |> count_timelines()
    |> Num.to_str()
    |> Stdout.line!()

parse_input : Str -> List Ascii.Ascii
parse_input = |input|
    input
    |> Str.trim_end()
    |> Str.split_on("\n")
    |> List.keep_oks(Ascii.from_str)

count_timelines : List Ascii.Ascii -> U64
count_timelines = |diagram|
    (count, _) = beam_loop(diagram, 0, Dict.empty({}))
    count

beam_loop : List Ascii.Ascii, U64, Dict (Str, U64) U64 -> (U64, Dict (Str, U64) U64)
beam_loop = |diagram, row_index, calculated_timelines|
    when diagram is
        [cur, next, .. as tail] ->
            when Dict.get(calculated_timelines, (Ascii.to_str(cur), row_index)) is
                Ok(n) -> (n, calculated_timelines)
                _ ->
                    quantum_beam_loop_step(cur, next)
                    |> Result.with_default([])
                    |> List.walk(
                        (0, calculated_timelines),
                        |(timeline_count, cur_calculated_timelines), next_timeline|
                            (new_timeline_count, new_calculated_timelines) =
                                beam_loop(
                                    tail |> List.prepend(next_timeline),
                                    row_index + 1,
                                    cur_calculated_timelines,
                                )

                            (
                                timeline_count + new_timeline_count,
                                new_calculated_timelines
                                |> Dict.insert((Ascii.to_str(next_timeline), row_index + 1), new_timeline_count),
                            ),
                    )

        _ -> (1, calculated_timelines)

quantum_beam_loop_step : Ascii.Ascii, Ascii.Ascii -> Result (List Ascii.Ascii) _
quantum_beam_loop_step = |cur, next|
    beam_pos = cur |> List.find_first_index(is_beam)?
    pipe_char = Char.from_ascii_byte(124)?
    next_at_beam_pos = next |> List.get(beam_pos) |> Result.map_ok(|c| Ascii.to_str([c]))?

    when next_at_beam_pos is
        "^" -> Ok([List.set(next, beam_pos - 1, pipe_char), List.set(next, beam_pos + 1, pipe_char)])
        _ -> Ok([List.set(next, beam_pos, pipe_char)])

is_beam : Char.Char -> Bool
is_beam = |char|
    str_char = Ascii.to_str([char])
    str_char == "S" or str_char == "|"
