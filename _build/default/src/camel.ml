open! Base

type direction =
  | Left
  | Up
  | Right
  | Down
[@@deriving sexp_of]

type caravan = {
  (* [orientation] represents the direction of the first camel. *)
  orientation : direction;
  (* [camels_left] represents the number of camels we need to add to the
     caravan. *)
  camels_left : int;
  (* [locations] represents the current set of squares that the caravan
     occupies. *)
  locations : Position.t list;
  (* [cacti_eaten] represents the number of cacti that the caravan has
     eaten) *)
  cacti_eaten : int;
}
[@@deriving sexp_of]

let create num_camels =
  {
    orientation = Right;
    camels_left = 0;
    cacti_eaten = 0;
    locations =
      List.init num_camels ~f:(fun col -> { Position.row = 0; col })
      |> List.rev;
  }

let add_camel c =
  {
    c with
    camels_left = c.camels_left + 1;
    cacti_eaten = c.cacti_eaten + 1;
  }

let first_camel c = List.hd_exn c.locations
let orientation c = c.orientation
let camels_left c = c.camels_left
let cacti_eaten c = c.cacti_eaten
let locations c = c.locations
let set_direction c d = { c with orientation = d }
let get_direction c = c.orientation
let set_locations c pos_list = { c with locations = pos_list }

let next_position d { Position.row; col } : Position.t =
  match d with
  | Left ->
      let new_cols = col - 1 in
      { row; col = new_cols }
  | Right ->
      let new_cols = col + 1 in
      { row; col = new_cols }
  | Up ->
      let new_rows = row + 1 in
      { row = new_rows; col }
  | Down ->
      let new_rows = row - 1 in
      { row = new_rows; col }

let remove_last_elt lst =
  match List.rev lst with
  | [] -> []
  | _ :: xs -> List.rev xs

let step ({ orientation; camels_left; locations; cacti_eaten } as c) =
  let body, camels_left =
    if camels_left > 0 then (locations, camels_left - 1)
    else (remove_last_elt locations, camels_left)
  in
  let new_first_camel = next_position orientation (first_camel c) in
  let check_collisions =
    List.mem body new_first_camel ~equal:Position.equal_position
  in
  match check_collisions with
  | true -> None
  | false ->
      Some
        {
          c with
          locations = new_first_camel :: body;
          camels_left;
          cacti_eaten;
        }
