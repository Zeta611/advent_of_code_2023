open Core
open Advent_of_code_2023.Exceptions

type map = record list
and record = { destination : int; source : int; length : int }
and seed = Just of int | Range of int * int
and seed_lst = seed list
and info = { seeds : seed list; maps : map list } [@@deriving sexp]

let parse_seeds (line : string) ~(part : int) : seed list =
  let nums =
    String.split line ~on:':'
    |> (fun lst -> List.nth_exn lst 1)
    |> String.split ~on:' '
    |> List.filter ~f:(fun str -> not (String.is_empty str))
    |> List.map ~f:Int.of_string
  in
  match part with
  | 1 -> List.map nums ~f:(fun n -> Just n)
  | 2 -> (
      let open List.Let_syntax in
      match%map List.groupi nums ~break:(fun i _ _ -> i mod 2 = 0) with
      | [ seed; length ] -> Range (seed, length)
      | _ -> failwith "Invalid input")
  | _ -> raise Invalid_part

let parse (lines : string list) ~(part : int) : info =
  let seeds = parse_seeds (List.nth_exn lines 0) ~part in
  let maps = Array.create ~len:7 [] in
  let i = ref 0 in
  List.iter
    (List.tl_exn @@ List.tl_exn lines)
    ~f:(fun line ->
      if String.is_empty line then i := !i + 1
      else if not (String.contains line ':') then
        let lst = String.split line ~on:' ' |> List.map ~f:Int.of_string in
        let record =
          {
            destination = List.nth_exn lst 0;
            source = List.nth_exn lst 1;
            length = List.nth_exn lst 2;
          }
        in
        maps.(!i) <- record :: maps.(!i));
  Array.iteri maps ~f:(fun i lst ->
      maps.(i) <-
        List.sort lst ~compare:(fun { source = n1; _ } { source = n2; _ } ->
            Int.compare n1 n2));
  { seeds; maps = List.of_array maps }

let lowest_location (lines : string list) ~(part : int) : int =
  let info = parse lines ~part in
  (* printf "%s\n" (Sexp.to_string_hum (sexp_of_info info)); *)
  let locations =
    let open List.Let_syntax in
    match%map info.seeds with
    | Just seed ->
        List.fold info.maps ~init:seed ~f:(fun num map ->
            match
              List.find map ~f:(fun { source; length; _ } ->
                  num >= source && num < source + length)
            with
            | Some { destination; source; _ } -> destination + (num - source)
            | None -> num)
    | Range _ as seed_range -> (
        let location_ranges =
          (* pipeline the seed range into the maps *)
          List.fold info.maps ~init:[ seed_range ] ~f:(fun source_ranges map ->
              let open List.Let_syntax in
              match%bind source_ranges with
              | Just _ -> failwith "Invalid input"
              | Range _ as source_range ->
                  let transformed, leftovers =
                    (* pipeline the range into the records *)
                    List.fold map ~init:([], [ source_range ])
                      ~f:(fun
                          (transformed, leftovers)
                          { destination; source; length }
                        ->
                        (* at most one of the leftovers will intersect with the current record, as the records are disjoint *)
                        let i_leftover =
                          List.findi leftovers ~f:(fun _ leftover ->
                              match leftover with
                              | Just _ -> failwith "Invalid input"
                              | Range (source', _length') ->
                                  source' >= source && source' < source + length)
                        in
                        match i_leftover with
                        | Some (_, Just _) -> failwith "Invalid input"
                        | Some (i, Range (source', length')) ->
                            let left_split, right_split =
                              List.split_n leftovers i
                            in
                            let leftovers =
                              left_split @ List.tl_exn right_split
                            in
                            (* Get [source', source'+length') /\ [source, source+length) *)
                            let left, right =
                              ( Int.max source source',
                                Int.min (source + length) (source' + length') )
                            in
                            let length = right - left in
                            if length <= 0 then failwith "programming error"
                            else
                              let gen_range s l =
                                if l <= 0 then [] else [ Range (s, l) ]
                              in
                              let left_leftover =
                                gen_range source' (left - source')
                              and right_leftover =
                                gen_range right (source' + length' - right)
                              in
                              ( [ Range (destination + (left - source), length) ]
                                @ transformed,
                                left_leftover @ right_leftover @ leftovers )
                        | None -> (transformed, leftovers))
                  in
                  transformed @ leftovers)
        in
        let min_location_range =
          List.min_elt location_ranges ~compare:(fun r1 r2 ->
              match (r1, r2) with
              | Range (n1, _), Range (n2, _) -> Int.compare n1 n2
              | _, _ -> failwith "Invalid input")
          |> Option.value_exn
        in
        match min_location_range with
        | Range (n, _) -> n
        | _ -> failwith "Invalid input")
  in
  List.min_elt locations ~compare:Int.compare |> Option.value_exn

let run part =
  let lines =
    In_channel.with_file "./inputs/5/input.txt" ~f:In_channel.input_lines
  in
  printf "%i\n" (lowest_location ~part lines)
