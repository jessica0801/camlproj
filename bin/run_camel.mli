open! Base
open! Camel_lib

val every : float -> f:(unit -> unit) -> stop:bool ref -> unit
(** [every s f stop] updates the game every s seconds. *)

val handle_keys : Game.t -> game_over:bool ref -> unit
(** [handle_keys game gameover] takes in keyboard input and translates
    it to arguments for functions that update the game progress*)

val handle_steps : Game.t -> game_over:bool ref -> unit
(** [handle_steps] takes in the game state and determines whether or not
    the game is over. *)

val run : unit -> unit
(** [run] is in charge of the entire running of the game. *)
