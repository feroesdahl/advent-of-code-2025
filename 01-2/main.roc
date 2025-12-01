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
      when rotate_str(head, pos) is
        Ok((next_pos, add)) -> loop(tail, next_pos, acc + add)
        _ -> Err("oh no")

rotate_str : Str, I32 -> Result (I32, I32) _
rotate_str = |rotation, pos|
  when parse_rotation_str(rotation) is
    Ok(("R", n)) -> Ok(rotate_n(n, pos))
    Ok(("L", n)) -> Ok(rotate_n(-n, pos))
    _ -> Err("oh no")

rotate_n : I32, I32 -> (I32, I32)
rotate_n = |rotation, pos|
  full_rotations = Num.abs(rotation // 100)
  remainder = rotation % 100
  next_pos = pos + remainder

  if remainder == 0 then
    (pos, full_rotations)
  else if next_pos == 0 then 
    (next_pos, full_rotations + 1)
  else if next_pos < 0 && pos != 0 then
    (next_pos + 100, full_rotations + 1)
  else if next_pos < 0 && pos == 0 then
    (next_pos + 100, full_rotations)
  else if next_pos >= 100 then
    (next_pos - 100, full_rotations + 1)
  else
    (next_pos, full_rotations)

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

