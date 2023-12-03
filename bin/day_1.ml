open Core
open Advent_of_code_2023.Exceptions

let first_and_last_digit_1 line =
  let search = String.find_map ~f:Char.get_digit in
  let first = search line in
  let last = search (String.rev line) in
  let first, last =
    match (first, last) with
    | Some first, Some last -> (first, last)
    | _ -> failwith "No digits found"
  in
  (first * 10) + last

(** zero is not used---just for completeness *)
let digits_map =
  [|
    "zero";
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine";
  |]

let rev_digits_map =
  [|
    "orez";
    "eno";
    "owt";
    "eerht";
    "ruof";
    "evif";
    "xis";
    "neves";
    "thgie";
    "enin";
  |]

let first_and_last_digit_2 line =
  let search line ~digits_map =
    let indices =
      let open List.Let_syntax in
      let%bind digit = List.range 0 10 in
      let match_digit =
        match String.index line (Char.of_int_exn (digit + Char.to_int '0')) with
        | Some i -> [ (digit, i) ]
        | None -> []
      in
      let match_spelled =
        match String.substr_index line ~pattern:digits_map.(digit) with
        | Some i -> [ (digit, i) ]
        | None -> []
      in
      match_digit @ match_spelled
    in

    let open Option.Let_syntax in
    let%bind p =
      List.min_elt indices ~compare:(fun (_, i1) (_, i2) -> Int.compare i1 i2)
    in
    Some (fst p)
  in
  let first = search line ~digits_map in
  let last = search (String.rev line) ~digits_map:rev_digits_map in
  let first, last =
    match (first, last) with
    | Some first, Some last -> (first, last)
    | _ -> failwith "No digits found"
  in
  (first * 10) + last

let run part =
  let lines =
    In_channel.with_file "./inputs/1/input.txt" ~f:In_channel.input_lines
  in
  let first_and_last_digit =
    match part with
    | 1 -> first_and_last_digit_1
    | 2 -> first_and_last_digit_2
    | _ -> raise Invalid_part
  in
  let value =
    List.fold lines ~init:0 ~f:(fun acc line -> acc + first_and_last_digit line)
  in
  printf "%i\n" value
