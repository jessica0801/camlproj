open! Base

type t = {
  col : int;
  row : int;
}
[@@deriving compare, sexp]

(* A helper function that defines equality for two camel positions. *)
let equal_position pos1 pos2 =
  let col1 = pos1.col in
  let col2 = pos2.col in
  let row1 = pos1.row in
  let row2 = pos2.row in
  col1 = col2 && row1 = row2
