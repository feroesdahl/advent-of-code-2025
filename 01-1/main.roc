app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br" }

import cli.Stdout
import cli.Arg exposing [Arg]

import "input.txt" as input_str : Str

main! : List Arg => Result {} _
main! = |_args|
  input = List.drop_last(Str.split_on(input_str, "\n"), 1)
  when loop(input, 50, 0) is
    Ok(n) -> Stdout.line!(Num.to_str(n))
    _ -> Stdout.line!("oh no")

loop : List Str, I32, I32 -> Result I32 _
loop = |rotations, pos, acc|
  when rotations is 
    [] -> Ok(acc)
    [head, .. as tail] -> 
      when rotate(head, pos) is
        Ok(0) -> loop(tail, 0, acc + 1)
        Ok(n) -> loop(tail, n, acc)
        _ -> Err("oh no")

rotate : Str, I32 -> Result I32 _
rotate = |rotation, pos|
  when parse_rotation_str(rotation) is
    Ok(("R", n)) -> Ok(positive_modulo_100(pos + n))
    Ok(("L", n)) -> Ok(positive_modulo_100(pos - n))
    _ -> Err("oh no")

parse_rotation_str : Str -> Result (Str, I32) _
parse_rotation_str = |rotation|
  if Str.starts_with(rotation, "R") then
    str = Str.drop_prefix(rotation, "R")
    when Str.to_i32(str) is
      Ok(n) -> Ok(("R", n))
      _ -> Err("oh no")
  else if Str.starts_with(rotation, "L") then
    str = Str.drop_prefix(rotation, "L")
    when Str.to_i32(str) is
      Ok(n) -> Ok(("L", n))
      _ -> Err("oh no")
  else 
    Err("oh no")

positive_modulo_100 : I32 -> I32
positive_modulo_100 = |n|
  m = n % 100
  if m < 0 then
    m + 100
  else
    m
