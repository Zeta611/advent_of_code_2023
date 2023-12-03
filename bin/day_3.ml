open Core
open Advent_of_code_2023.Exceptions

let left_adjacent (line : string) (y : int) =
  let j = ref y in
  while !j > 0 && Char.is_digit line.[!j - 1] do
    j := !j - 1
  done;
  if y = !j then 0 else Int.of_string (String.sub line ~pos:!j ~len:(y - !j))

let right_adjacent (line : string) (y : int) =
  let j = ref y in
  while !j < String.length line - 1 && Char.is_digit line.[!j + 1] do
    j := !j + 1
  done;
  if y = !j then 0
  else Int.of_string (String.sub line ~pos:(y + 1) ~len:(!j - y))

let left_right_adjacent (line : string) (y : int) =
  let j = ref y in
  let left_edge =
    while !j > 0 && Char.is_digit line.[!j - 1] do
      j := !j - 1
    done;
    !j
  and right_edge =
    j := y;
    while !j < String.length line - 1 && Char.is_digit line.[!j + 1] do
      j := !j + 1
    done;
    !j
  in
  Int.of_string
    (String.sub line ~pos:left_edge ~len:(right_edge - left_edge + 1))

let add_adjacents (grid : string list) (x : int) (y : int) =
  let line = List.nth_exn grid x in
  let left = left_adjacent line y
  and right = right_adjacent line y
  and top =
    match List.nth grid (x - 1) with
    | None -> 0
    | Some line ->
        if Char.is_digit line.[y] then left_right_adjacent line y
        else left_adjacent line y + right_adjacent line y
  and bottom =
    match List.nth grid (x + 1) with
    | None -> 0
    | Some line ->
        if Char.is_digit line.[y] then left_right_adjacent line y
        else left_adjacent line y + right_adjacent line y
  in
  left + right + top + bottom

let get_gear_ratio (grid : string list) (x : int) (y : int) =
  let line = List.nth_exn grid x in
  let left = [ left_adjacent line y ]
  and right = [ right_adjacent line y ]
  and top =
    match List.nth grid (x - 1) with
    | None -> []
    | Some line ->
        if Char.is_digit line.[y] then [ left_right_adjacent line y ]
        else [ left_adjacent line y; right_adjacent line y ]
  and bottom =
    match List.nth grid (x + 1) with
    | None -> []
    | Some line ->
        if Char.is_digit line.[y] then [ left_right_adjacent line y ]
        else [ left_adjacent line y; right_adjacent line y ]
  in
  let all =
    List.filter (left @ right @ top @ bottom) ~f:(fun n -> not (Int.equal 0 n))
  in
  if List.length all = 2 then List.fold all ~init:1 ~f:( * ) else 0

let count_parts (grid : string list) =
  List.foldi grid ~init:0 ~f:(fun i acc line ->
      String.foldi line ~init:acc ~f:(fun j acc c ->
          if not (Char.is_digit c || Char.equal c '.') then
            acc + add_adjacents grid i j
          else acc))

let count_gear_ratios (grid : string list) =
  List.foldi grid ~init:0 ~f:(fun i acc line ->
      String.foldi line ~init:acc ~f:(fun j acc c ->
          if Char.equal c '*' then acc + get_gear_ratio grid i j else acc))

let run part =
  let grid =
    In_channel.with_file "./inputs/3/input.txt" ~f:In_channel.input_lines
  in
  let count =
    match part with
    | 1 -> count_parts
    | 2 -> count_gear_ratios
    | _ -> raise Invalid_part
  in
  printf "%i\n" (count grid)
