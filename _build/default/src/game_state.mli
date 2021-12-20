open! Base

(** [t] is the current state of the game. *)
type t =
  | Start
  | Playing
  | Game_over of string
  | Win
[@@deriving sexp_of]

val to_string : t -> string
(** [to_string t] pretty prints the current game state as a string*)
