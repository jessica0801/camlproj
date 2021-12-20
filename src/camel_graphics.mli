open! Base

val init : unit -> Game.t
(** [init] initializes game, fails if called more than once.*)

val render : Game.t -> unit
(** [render t] renders the game board *)

val input : unit -> char option
(** [input] returns keyboard input if available. *)
