open! Base

type t = {
  col : int;
  row : int;
}
[@@deriving compare, sexp]
(** [t] represents position of an element through row and column value
    on grid.*)

val equal_position : t -> t -> bool
(** [equal_position pos1 pos2] returns true if the positions of the
    first camels are equal. Otherwise, it returns false. *)
