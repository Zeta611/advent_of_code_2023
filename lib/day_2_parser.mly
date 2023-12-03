%{
open Day_2_syntax

type color = Red | Green | Blue
and cubes = { color : color; count : int }

let list_to_set (cubes_list : cubes list) =
  let red = ref 0
  and green = ref 0
  and blue = ref 0 in
  List.iter (fun { color; count } ->
    match color with
    | Red -> red := !red + count
    | Green -> green := !green + count
    | Blue -> blue := !blue + count
  ) cubes_list;
  { red = !red; green = !green; blue = !blue }
%}


%token <int> NUM
%token GAME
%token RED GREEN BLUE
%token COLON SEMI COMMA
%token EOF

%start <game list> games

%%

games:
  | line* EOF
    { $1 }

line:
  | GAME NUM COLON record
    { { id = $2; record = $4 } }

record:
  | separated_nonempty_list(SEMI, set)
    { $1 }

set:
  | separated_nonempty_list(COMMA, cubes)
    { list_to_set $1 }

cubes:
  | NUM color
    { { color = $2; count = $1 } }

color:
  | RED
    { Red }
  | GREEN
    { Green }
  | BLUE
    { Blue }
