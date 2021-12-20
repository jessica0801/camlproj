open! Base

exception InvalidSize

type t [@@deriving sexp_of]

val create :
  height:int -> width:int -> not_possible:Position.t list -> t option
(** [create h w n] has input all the [Camel.position] elements in the
    desert that the cactus cannot be placed and [height] and [width] of
    the area where a cactus can be created, and creates a [Cactus.t].

    [create] returns [None] if there are no valid positions for the
    cactus or if the cactus size is too large. *)

val location : t -> Position.t
(** [location t] returns the position of the cactus in the desert. *)
