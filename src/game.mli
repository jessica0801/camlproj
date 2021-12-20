open! Base

type t [@@deriving sexp_of]
(** A [t] represents the whole game state, which is composed of the
    current caravan, cactus, and game state. *)

val in_bounds : t -> Position.t -> bool
(** [in_bounds game { Position.row; col }] is whether the position is
    within the valid bounds of the game. It returns [true] if the
    position is valid, false otherwise. *)

val create : height:int -> width:int -> initial_caravan_length:int -> t
(** [create ~height ~width ~initial_caravan_lengthh] creates a new game
    with specified parameters. *)

val width : t -> int
(** [get_width game] gets the width of the game *)

val height : t -> int
(** [get_height game] gets the height of the game *)

val caravan : t -> Camel.caravan
(** [get_caravan game] is the caravan of camels that is currently in the
    game. *)

val cactus : t -> Cactus.t
(** [get_cactus game] returns the cactus currently in the game *)

val set_direction : t -> Camel.direction -> unit
(** [set_directon game d] sets a new direction that the caravan faces. *)

val set_dir_game : t -> Camel.direction -> t
(** [set_dir_game game d] sets a new direction that the caravan faces
    and returns the game itself. *)

val get_direction : t -> Camel.direction
(** [get_direction game] gets the new direction that the caravan faces. *)

val get_string_game_state : t -> string
(** [get_game_state game] gets the string of the current state of the
    game. *)

val game_state : t -> Game_state.t
(** [game_state game] is the current state of the game. *)

val step : t -> unit
(** [step game] called within a loop, with the game re-rendering after
    each call*)
