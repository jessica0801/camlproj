open! Base

(** [direction] represents a direction. *)
type direction =
  | Left
  | Up
  | Right
  | Down
[@@deriving sexp_of]

type caravan [@@deriving sexp_of]
(** A [t] represents a caravan of camels in the desert. *)

val create : int -> caravan
(** [create i] makes a new caravan with the given number of camels. The
    length must be positive.

    The caravan will initially occupy the (column, row) locations:
    (0,0), (1,0), (2,0), ..., (length - 1, 0)

    The front of the caravan will be at position (length - 1, 0) and the
    initial direction will be facing the right. *)

val add_camel : caravan -> caravan
(** [add_camel c] adds one camel to camels left and cacti eaten. *)

val first_camel : caravan -> Position.t
(** [first_camel c] returns the location of the head of the caravan. *)

val orientation : caravan -> direction
(** [orientation c] returns the current direction the camel is facing. *)

val camels_left : caravan -> int
(** [camels_left c] returns the number of camels to be added to the
    caravan. *)

val cacti_eaten : caravan -> int
(** [cacti_eaten c] returns the number of cacti that has been eaten in
    the game *)

val locations : caravan -> Position.t list
(** [locations c] returns the current locations that the caravan
    occupies. The first element of the list is the first camel.
    [locations] cannot be empty. *)

val set_direction : caravan -> direction -> caravan
(** [set_direction c d] makes the caravan move in a specific direction
    when [step] is called. *)

val get_direction : caravan -> direction
(** [get_direction c] returns the direction that the caravan moves in
    when [step] is called *)

val set_locations : caravan -> Position.t list -> caravan
(** [set_locations c pos_list] makes the caravan move to location
    represented by inputed position list*)

val next_position : direction -> Position.t -> Position.t
(** next_position d { Position.row; col }] takes a direction and a
    starting position and returns the next position after taking one
    step in the specified direction. *)

val step : caravan -> caravan option
(** [step c] moves the caravan forward 1 step. [step] returns [None] if
    the first camel collides with rest of the caravan. *)
