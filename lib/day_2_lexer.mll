{
  open Day_2_parser
  exception LexError of string
}

let blank = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"

let digit = ['0'-'9']
let number = '-'? digit+

rule token = parse
| blank
    { token lexbuf }
| newline
    { token lexbuf }
| eof
    { EOF }
| number as n
    { NUM (int_of_string n) }
| "Game"
    { GAME }
| "red"
    { RED }
| "green"
    { GREEN }
| "blue"
    { BLUE }
| ":"
    { COLON }
| ";"
    { SEMI }
| ","
    { COMMA }
| _
    { raise (LexError (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
