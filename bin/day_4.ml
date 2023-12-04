open Core
open Advent_of_code_2023.Exceptions

exception Invalid_card

let count_winning_cards (card : string) : int =
  let numbers = List.nth_exn (String.split card ~on:':') 1 in
  let winning_and_my_numbers = String.split numbers ~on:'|' in
  match winning_and_my_numbers with
  | [ winning_numbers; my_numbers ] ->
      let f line =
        String.split ~on:' ' line
        |> List.filter ~f:(fun s -> not (String.is_empty s))
        |> List.map ~f:Int.of_string
        |> Set.of_list (module Int)
      in
      let winning_numbers, my_numbers = (f winning_numbers, f my_numbers) in
      Set.length @@ Set.inter winning_numbers my_numbers
  | _ -> raise Invalid_card

let count_points (cards : string list) : int =
  List.sum
    (module Int)
    cards
    ~f:(fun card ->
      let count = count_winning_cards card in
      if count = 0 then 0 else Int.pow 2 (count - 1))

let count_all_cards (cards : string list) : int =
  let length = List.length cards in
  let counts = Array.create ~len:length 1 in
  List.iteri cards ~f:(fun i card ->
      let count = count_winning_cards card in
      for copy = i + 1 to Int.min (i + count) (length - 1) do
        counts.(copy) <- counts.(copy) + counts.(i)
      done);
  Array.sum (module Int) counts ~f:Fn.id

let run part =
  let cards =
    In_channel.with_file "./inputs/4/input.txt" ~f:In_channel.input_lines
  in
  let count_points =
    match part with
    | 1 -> count_points
    | 2 -> count_all_cards
    | _ -> raise Invalid_part
  in
  printf "%i\n" (count_points cards)
