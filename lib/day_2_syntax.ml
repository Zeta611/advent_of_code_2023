open Core

type set = { red : int; green : int; blue : int }
and game = { id : int; record : set list } [@@deriving sexp]
