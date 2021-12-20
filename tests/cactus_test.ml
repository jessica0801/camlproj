open! Base
open! Camel_lib
open OUnit2
open Cactus

let all_locations =
  List.init 10 ~f:(fun row ->
      List.init 10 ~f:(fun col -> { Position.row; col }))
  |> List.concat

(* test create *)
let create_none_test
    (name : string)
    (height : int)
    (width : int)
    (not_possible : Position.t list)
    (expected_output : Cactus.t option) : test =
  name >:: fun _ ->
  assert_equal expected_output (create ~height ~width ~not_possible)

let create_some_test
    (name : string)
    (height : int)
    (width : int)
    (not_possible : Position.t list)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (try
       let cactus = create ~height ~width ~not_possible in
       match cactus with
       | None -> false
       | Some _ -> true
     with
    | InvalidSize -> false)

let create_exn_test
    (name : string)
    (height : int)
    (width : int)
    (not_possible : Position.t list)
    (expected_exn : exn) : test =
  name >:: fun _ ->
  assert_raises expected_exn (fun () ->
      create ~height ~width ~not_possible)

let create_none_tests =
  [
    create_none_test "None cactus when all locations not possible" 10 10
      all_locations None;
    create_none_test
      "Negative width for game board creates a None cactus" ~-10 10 []
      None;
    create_none_test
      "Negative height for game board creates a None cactus" 10 ~-10 []
      None;
    create_none_test
      "Negative width and height for game board creates a None cactus"
      ~-10 ~-10 [] None;
    create_none_test
      "Zero width and negative height for game board creates a None \
       cactus"
      0 ~-10 [] None;
    create_none_test
      "Zero height and negative width for game board creates a None \
       cactus"
      ~-10 0 [] None;
    create_none_test "Zero width for game board creates a None cactus" 0
      10 [] None;
    create_none_test "Zero height for game board creates a None cactus"
      10 0 [] None;
    create_none_test
      "Zero width and height 10 for game board creates a None cactus" 0
      10 [] None;
    create_none_test
      "Zero width and height 1 for game board creates a None cactus" 0 1
      [] None;
    create_none_test
      "Width 1 and height 0 for game board creates a None cactus" 1 0 []
      None;
  ]

let create_some_tests =
  [
    create_some_test "Game board width too large returns true" 10 200 []
      true;
    create_some_test "Game board width too large returns true" 10 300 []
      true;
    create_some_test "Game board width too large returns true" 10 200 []
      true;
    create_some_test "Game board height too large returns true" 200 10
      [] true;
    create_some_test "Game board height too large returns true" 300 10
      [] true;
    create_some_test
      "Game board width and height too large returns true" 200 200 []
      true;
    create_some_test
      "Returns true for Some cactus when possible locations and valid \
       game board"
      10 10 [] true;
    create_some_test
      "Returns true for Some cactus when possible locations and \
       smallest valid game board"
      1 1 [] true;
    create_some_test
      "Returns false when no possible locations and valid game board \
       size"
      10 10 all_locations false;
    create_some_test
      "Returns true when possible cactus locations and invalid game \
       board"
      10 200 [] true;
  ]

let create_excn_tests = []

let suite =
  "test suite for Cactus"
  >::: Stdlib.List.flatten
         [ create_some_tests; create_none_tests; create_excn_tests ]

let _ = run_test_tt_main suite
