open Core
open Advent_of_code_2023
open Advent_of_code_2023.Day_2_syntax
open Advent_of_code_2023.Exceptions

let max_red, max_green, max_blue = (12, 13, 14)

let search_possible (games : game list) : int list =
  let set_condition set =
    set.red <= max_red && set.green <= max_green && set.blue <= max_blue
  in
  let record_condition record = List.for_all record ~f:set_condition in
  List.filter_map games ~f:(fun game ->
      if record_condition game.record then Some game.id else None)

let power_minimum (games : game list) : int list =
  let min_set_for_record record =
    List.fold record ~init:{ red = 0; green = 0; blue = 0 } ~f:(fun acc set ->
        let red = Int.max set.red acc.red
        and green = Int.max set.green acc.green
        and blue = Int.max set.blue acc.blue in
        { red; green; blue })
  in
  List.map games ~f:(fun game ->
      let min_set = min_set_for_record game.record in
      min_set.red * min_set.green * min_set.blue)

let run part =
  let games =
    In_channel.with_file "./inputs/2/input.txt" ~f:(fun file ->
        Day_2_parser.games Day_2_lexer.token (Lexing.from_channel file))
  in
  let search_possible =
    match part with
    | 1 -> search_possible
    | 2 -> power_minimum
    | _ -> raise Invalid_part
  in
  let value = List.sum (module Int) (search_possible games) ~f:Fn.id in
  printf "%i\n" value
