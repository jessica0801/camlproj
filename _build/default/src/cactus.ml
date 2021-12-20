open! Base

exception InvalidSize

type t = { location : Position.t } [@@deriving sexp_of]

let location t = t.location

let create ~height ~width ~not_possible =
  let possible =
    List.concat_map (List.range 0 height) ~f:(fun row ->
        let range = List.range 0 width in
        List.map range ~f:(fun col -> { Position.row; col }))
    |> List.filter ~f:(fun pos ->
           not
             (List.mem not_possible pos ~equal:Position.equal_position))
  in
  match possible with
  | [] -> None
  | _ ->
      let loc = { location = List.random_element_exn possible } in
      Some loc
