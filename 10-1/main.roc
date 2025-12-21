app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.3.1/1PCTQ0tzSijxfhxDg1k_yPtfOXiAk3j283b8EWGusVc.tar.br",
}

import cli.Stdout
import cli.Arg exposing [Arg]
import ascii.Ascii
import ascii.Char

import "input.txt" as input_str : Str

Machine : { indicators : Set U64, buttons : List (List U64) }

main! : List Arg => Result {} _
main! = |_args|
    machines = input_str |> parse_input()?

    machines
    |> List.map(calculate_min_button_presses)
    |> List.sum()
    |> Num.to_str()
    |> Stdout.line!()

calculate_min_button_presses : Machine -> U64
calculate_min_button_presses = |machine|
    loop = |state, buttons, pressed, best_min|
        when buttons is
            _ if pressed == best_min - 1 -> best_min
            [button, .. as tail] ->
                pressed_state = press_button(state, button)
                if pressed_state == machine.indicators then
                    pressed + 1
                else
                    pressed_result = loop(pressed_state, tail, pressed + 1, best_min)
                    loop(state, tail, pressed, pressed_result)

            _ -> best_min

    loop(Set.empty({}), machine.buttons, 0, Num.max_u64)

press_button : Set U64, List U64 -> Set U64
press_button = |state, button|
    button
    |> List.walk(
        state,
        |walk_state, button_toggle|
            if walk_state |> Set.contains(button_toggle) then
                walk_state |> Set.remove(button_toggle)
            else
                walk_state |> Set.insert(button_toggle),
    )

parse_input : Str -> Result (List Machine) _
parse_input = |input|
    input
    |> Str.trim_end()
    |> Str.split_on("\n")
    |> List.map_try(parse_row)

parse_row : Str -> Result Machine [InvalidAscii, InvalidNumStr, InvalidInput]
parse_row = |row|
    when row |> Str.split_on(" ") is
        [indicator_diagram, .. as button_wirings, _] ->
            Result.map2(
                parse_indicator_diagram(indicator_diagram),
                parse_button_wirings(button_wirings),
                |i, b| { indicators: i, buttons: b },
            )

        _ -> Err(InvalidInput)

parse_indicator_diagram : Str -> Result (Set U64) _
parse_indicator_diagram = |indicator_diagram|
    num_char = Char.from_ascii_byte(35)?

    indicator_diagram
    |> Ascii.from_str()
    |> Result.map_ok(
        |chars|
            chars
            |> List.drop_first(1)
            |> List.drop_last(1)
            |> List.walk_with_index(
                Set.empty({}),
                |acc, char, index|
                    if char == num_char then acc |> Set.insert(index) else acc,
            ),
    )

parse_button_wirings : List Str -> Result (List (List U64)) _
parse_button_wirings = |button_wirings|
    button_wirings |> List.map_try(parse_button_wiring)

parse_button_wiring : Str -> Result (List U64) _
parse_button_wiring = |button_wiring|
    comma_char = Char.from_ascii_byte(44)?

    button_wiring
    |> Ascii.from_str()
    |> Result.try(
        |chars|
            chars
            |> List.drop_first(1)
            |> List.drop_last(1)
            |> List.split_on(comma_char)
            |> List.map(Ascii.to_str)
            |> List.map_try(Str.to_u64),
    )
