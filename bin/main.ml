open Core
open Advent_of_code_2023.Exceptions

let drivers = [ Day_1.run; Day_2.run ]

let run_driver day_and_part =
  let day, part = day_and_part in
  let driver =
    try List.nth_exn drivers (day - 1) with _ -> raise Invalid_day
  in
  driver part

let param =
  let open Command.Param in
  both (anon ("day" %: int)) (anon ("part" %: int))

let command =
  Command.basic ~summary:"Advent of Code 2023"
    (Command.Param.map param ~f:(fun day_and_part () -> run_driver day_and_part))

let () = Command_unix.run command
