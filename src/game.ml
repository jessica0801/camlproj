open! Base

type t = {
  mutable caravan : Camel.caravan;
  mutable cactus : Cactus.t;
  mutable game_state : Game_state.t;
  height : int;
  width : int;
}
[@@deriving sexp_of]

let in_bounds t { Position.row; col } =
  col >= 0 && col < t.width && row >= 0 && row < t.height

let create ~height ~width ~initial_caravan_length =
  let caravan = Camel.create initial_caravan_length in
  let cactus =
    Cactus.create ~height ~width ~not_possible:(Camel.locations caravan)
  in
  match cactus with
  | None -> failwith "Unable to create initial cactus"
  | Some cactus ->
      let t =
        { caravan; cactus; game_state = Playing; height; width }
      in
      if
        List.exists (Camel.locations caravan) ~f:(fun pos ->
            not (in_bounds t pos))
      then failwith "Unable to create initial caravan"
      else t

let height t = t.height
let width t = t.width
let caravan t = t.caravan
let cactus t = t.cactus
let game_state t = t.game_state

let set_direction t direction =
  t.caravan <- Camel.set_direction t.caravan direction

let set_dir_game t direction =
  t.caravan <- Camel.set_direction t.caravan direction;
  t

let get_direction t = Camel.get_direction t.caravan

let get_string_game_state t =
  match game_state t with
  | Start -> "Start"
  | Playing -> "Playing"
  | Game_over s -> s
  | Win -> "Win"

let maybe_eat_cactus t first_camel =
  if
    not
      ([%compare.equal: Position.t] first_camel
         (Cactus.location t.cactus))
  then ()
  else
    let caravan = Camel.add_camel t.caravan in
    let cactus =
      Cactus.create ~height:t.height ~width:t.width
        ~not_possible:(Camel.locations caravan)
    in
    match cactus with
    | None -> t.game_state <- Win
    | Some cactus ->
        t.caravan <- caravan;
        t.cactus <- cactus

let step t =
  match Camel.step t.caravan with
  | None -> t.game_state <- Game_over "Self collision"
  | Some caravan ->
      t.caravan <- caravan;
      let first_camel = Camel.first_camel caravan in
      if not (in_bounds t first_camel) then
        t.game_state <- Game_over "Wall collision"
      else maybe_eat_cactus t first_camel
