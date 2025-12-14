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
    |> count_splits()
    |> Num.to_str()
    |> Stdout.line!()

parse_input : Str -> List Ascii.Ascii
parse_input = |input|
    input
    |> Str.trim_end()
    |> Str.split_on("\n")
    |> List.keep_oks(Ascii.from_str)

count_splits : List Ascii.Ascii -> U64
count_splits = |diagram|
    beam_loop(diagram, 0)

beam_loop : List Ascii.Ascii, U64 -> U64
beam_loop = |diagram, split_count|
    when diagram is
        [cur, next, .. as tail] ->
            (new_next, new_splits) = beam_loop_step(cur, next) |> Result.with_default(([], split_count))
            beam_loop(tail |> List.prepend(new_next), split_count + new_splits)

        _ -> split_count

beam_loop_step : Ascii.Ascii, Ascii.Ascii -> Result (Ascii.Ascii, U64) _
beam_loop_step = |cur, next|
    beam_pos = List.walk_with_index(
        cur,
        Set.empty({}),
        |pos, char, index|
            if is_beam(char) then
                pos |> Set.insert(index)
            else
                pos,
    )

    pipe_char = Char.from_ascii_byte(124)?

    result =
        List.walk_with_index(
            next,
            (next, 0),
            |(diagram, splits), char, index|
                if Set.contains(beam_pos, index) then
                    when Ascii.to_str([char]) is
                        "^" ->
                            (diagram |> List.set(index - 1, pipe_char) |> List.set(index + 1, pipe_char), splits + 1)

                        _ ->
                            (diagram |> List.set(index, pipe_char), splits)
                else
                    (diagram, splits),
        )
    Ok(result)

is_beam : Char.Char -> Bool
is_beam = |char|
    str_char = Ascii.to_str([char])
    str_char == "S" or str_char == "|"
