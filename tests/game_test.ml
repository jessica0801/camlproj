open! Base
open! Camel_lib
open OUnit2
open Game
open Camel

(* test in_bounds *)
let testing_game =
  Game.create ~height:10 ~width:10 ~initial_caravan_length:1

let in_bounds_test
    (name : string)
    (game : Game.t)
    (position : Position.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (in_bounds game position)

let true_in_bounds_tests =
  [
    in_bounds_test
      "Returns true when Position.t is within height & width of \
       testing_game"
      testing_game { row = 0; col = 5 } true;
    in_bounds_test
      "Returns true when Position.t is within height & width of \
       testing_game"
      testing_game { row = 5; col = 0 } true;
    in_bounds_test
      "Returns true when Position.t is within height & width of \
       testing_game"
      testing_game { row = 0; col = 0 } true;
    in_bounds_test
      "Returns true when Position.t is within height & width of \
       testing_game"
      testing_game { row = 5; col = 5 } true;
    in_bounds_test
      "Returns true when Position.t is within height & width of \
       testing_game"
      testing_game { row = 6; col = 6 } true;
    in_bounds_test
      "Returns true when Position.t is within height & width of \
       testing_game"
      testing_game { row = 7; col = 7 } true;
    in_bounds_test
      "Returns true when Position.t is within height & width of \
       testing_game"
      testing_game { row = 8; col = 8 } true;
    in_bounds_test
      "Returns true when Position.t is within height & width of \
       testing_game"
      testing_game { row = 9; col = 9 } true;
  ]

let false_in_bounds_tests =
  [
    in_bounds_test "invalid, columns is equal to board width"
      testing_game { row = 5; col = 10 } false;
    in_bounds_test "invalid, rows is equal to board height" testing_game
      { row = 10; col = 5 } false;
    in_bounds_test "invalid, columns is less than 0" testing_game
      { row = 5; col = -1 } false;
    in_bounds_test "invalid, columns is less than 0" testing_game
      { row = 5; col = -100 } false;
    in_bounds_test
      "invalid, columns is greater than or equal to board width"
      testing_game { row = 5; col = 11 } false;
    in_bounds_test
      "invalid, columns is greater than or equal to board width"
      testing_game { row = 5; col = 111 } false;
    in_bounds_test "invalid, rows is less than 0" testing_game
      { row = -1; col = 5 } false;
    in_bounds_test "invalid, rows is less than 0" testing_game
      { row = -100; col = 5 } false;
    in_bounds_test
      "invalid, rows is greater than or equal to board height"
      testing_game { row = 11; col = 5 } false;
    in_bounds_test
      "invalid, rows is greater than or equal to board height"
      testing_game { row = 111; col = 5 } false;
  ]

(* test create *)

let test_width (width : int) (expected_width : int) : bool =
  width = expected_width

let test_height (height : int) (expected_height : int) : bool =
  height = expected_height

let test_length (length : int) (expected_length : int) : bool =
  length = expected_length

let create_height_test
    (name : string)
    (game : Game.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal (Game.height game) expected_output
    ~printer:Stdlib.string_of_int

let create_width_test
    (name : string)
    (game : Game.t)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal (Game.width game) expected_output
    ~printer:Stdlib.string_of_int

let game_one =
  Game.create ~height:100 ~width:450 ~initial_caravan_length:1

let game_two =
  Game.create ~height:400 ~width:450 ~initial_caravan_length:2

let game_three =
  Game.create ~height:400 ~width:450 ~initial_caravan_length:3

let game_five =
  Game.create ~height:400 ~width:450 ~initial_caravan_length:5

let game_ten =
  Game.create ~height:400 ~width:450 ~initial_caravan_length:10

let create_tests =
  [
    create_height_test "Test the height parameter vals for game one"
      game_one 100;
    create_height_test "Test the height parameter vals for game two"
      game_two 400;
    create_height_test "Test the height parameter vals for game three"
      game_three 400;
    create_height_test "Test the height parameter vals for game five"
      game_five 400;
    create_height_test "Test the height parameter vals for game ten"
      game_ten 400;
    create_width_test "Test the width parameter for game one" game_one
      450;
    create_width_test "Test the width parameter vals for game two"
      game_two 450;
    create_width_test "Test the width parameter vals for game three"
      game_three 450;
    create_width_test "Test the width parameter vals for game five"
      game_five 450;
    create_width_test "Test the width parameter vals for game ten"
      game_ten 450;
  ]

(* test set_direction *)
let set_direction_test
    (name : string)
    (t : Game.t)
    (d : Camel.direction)
    (expected_output : Camel.direction) : test =
  name >:: fun _ ->
  Game.set_direction t d;
  assert_equal expected_output (Game.get_direction t)

let set_direction_tests =
  [
    set_direction_test "Set direction of one camel to Right" game_one
      Right Right;
    set_direction_test "Set direction of caravan of one camel to Left"
      game_one Left Left;
    set_direction_test "Set direction of caravan of one camel to Up"
      game_one Up Up;
    set_direction_test "Set direction of caravan of one camel to Down"
      game_one Down Down;
    set_direction_test "Set direction of caravan of two camels to Right"
      game_two Right Right;
    set_direction_test "Set direction of caravan of two camels to Left"
      game_two Left Left;
    set_direction_test "Set direction of caravan of two camels to Up"
      game_two Up Up;
    set_direction_test "Set direction of caravan of two camels to Down"
      game_two Down Down;
    set_direction_test
      "Set direction of caravan of three camels to Right" game_three
      Right Right;
    set_direction_test
      "Set direction of caravan of three camels to Left" game_three Left
      Left;
    set_direction_test "Set direction of caravan of three camels to Up"
      game_three Up Up;
    set_direction_test
      "Set direction of caravan of three camels to Down" game_three Down
      Down;
    set_direction_test
      "Set direction of caravan of five camels to Right" game_five Right
      Right;
    set_direction_test "Set direction of caravan of five camels to Left"
      game_five Left Left;
    set_direction_test "Set direction of caravan of five camels to Up"
      game_five Up Up;
    set_direction_test
      "Set direction of caravan of ffiivvee camels to Down" game_five
      Down Down;
    set_direction_test "Set direction of caravan of ten camels to Right"
      game_ten Right Right;
    set_direction_test "Set direction of caravan of ten camels to Left"
      game_ten Left Left;
    set_direction_test "Set direction of caravan of ten camels to Up"
      game_ten Up Up;
    set_direction_test "Set direction of caravan of ten camels to Down"
      game_ten Down Down;
  ]

(* test step *)

(* let step_none_test (name : string) (t : Game.t) (expected_output :
   string) : test = name >:: fun _ -> Game.step t; assert_equal
   expected_output (Game.get_string_game_state t)

   let step_some_test (name : string) (t : Game.t) (expected_output :
   string) : test = name >:: fun _ -> Game.step t; assert_equal
   expected_output (Game.get_string_game_state t)

   (* game state changes to game over when none and when some, updated
   caravan, first camel, and check for wall collision *)

   let zero_camels = Camel.create 0 let one_camel = Camel.create 1 let
   two_camels = Camel.create 2 let three_camels = Camel.create 3 let
   five_camels = Camel.create 5 let eight_camels = Camel.create 8 let
   ten_camels = Camel.create 10

   (* left-facing three camel caravan with positions head=(0,0); (0,1);
   (0,2) *) let left_loc : Position.t list = [ { row = 0; col = 0 }; {
   row = 0; col = 1 }; { row = 0; col = 2 } ]

   (* up-facing three camel caravan with positions head=(2,0); (1,0);
   (0,0) *) let up_loc : Position.t list = [ { row = 2; col = 0 }; { row
   = 1; col = 0 }; { row = 0; col = 0 } ]

   (* down-facing three camel caravan with positions head=(0,0); (1,0);
   (2,0) *) let down_loc : Position.t list = [ { row = 0; col = 0 }; {
   row = 1; col = 0 }; { row = 2; col = 0 } ]

   (* three_left_camels is caravan with head @ (0,0) and body to the
   right of it*) let three_left_camels = Camel.set_locations
   three_camels left_loc

   (* three_up_camels is caravan with head @ (1,1) and body below it*)
   let three_up_camels = Camel.set_locations three_camels up_loc

   (* three_up_camels is caravan with head @ (0,0) and body above it*)
   let three_down_camels = Camel.set_locations three_camels down_loc

   let step_none_tests = [ step_none_test "Returns \"Self collision\"
   when stepping camel fails for \ game_one" (Game.set_dir_game game_one
   Left) "Self collision"; step_none_test "Returns \"Self collision\"
   when stepping camel fails for \ game_two" (Game.set_dir_game game_two
   Left) "Self collision"; step_none_test "Returns \"Self collision\"
   when stepping camel fails for \ game_three" (Game.set_dir_game
   game_three Left) "Self collision"; step_none_test "Returns \"Self
   collision\" when stepping camel fails for \ game_five"
   (Game.set_dir_game game_five Left) "Self collision"; step_none_test
   "Returns \"Self collision\" when stepping camel fails for \ game_ten"
   (Game.set_dir_game game_ten Left) "Self collision"; (*--*)
   step_none_test "Returns \"Self collision\" when stepping camel fails
   for \ game_one" (Game.set_dir_game game_one Down) "Wall collision";
   step_none_test "Returns \"Self collision\" when stepping camel fails
   for \ game_two" (Game.set_dir_game game_two Down) "Wall collision";
   step_none_test "Returns \"Self collision\" when stepping camel fails
   for \ game_three" (Game.set_dir_game game_three Down) "Wall
   collision"; step_none_test "Returns \"Self collision\" when stepping
   camel fails for \ game_five" (Game.set_dir_game game_five Right)
   "Wall collision"; step_none_test "Returns \"Self collision\" when
   stepping camel fails for \ game_ten" (Game.set_dir_game game_ten
   Down) "Wall collision"; ] *)

(* let step_some_tests = [ step_some_test "Returns \"Playing\" when
   stepping camel continues for game_one" (Game.set_dir_game game_one
   Right) "Playing"; step_some_test "Returns \"Playing\" when stepping
   camel continues for game_two" (Game.set_dir_game game_two Right)
   "Playing"; step_some_test "Returns \"Playing\" when stepping camel
   continues for game_three" (Game.set_dir_game game_three Right)
   "Playing"; step_some_test "Returns \"Playing\" when stepping camel
   continues for game_five" (Game.set_dir_game game_five Right)
   "Playing"; step_some_test "Returns \"Playing\" when stepping camel
   continues for game_ten" (Game.set_dir_game game_ten Right) "Playing";
   step_some_test "Returns \"Playing\" when stepping camel continues for
   game_one" (Game.set_dir_game game_one Up) "Playing"; step_some_test
   "Returns \"Playing\" when stepping camel continues for game_two"
   (Game.set_dir_game game_two Up) "Playing"; step_some_test "Returns
   \"Playing\" when stepping camel continues for game_three"
   (Game.set_dir_game game_three Up) "Playing"; step_some_test "Returns
   \"Playing\" when stepping camel continues for game_five"
   (Game.set_dir_game game_five Up) "Playing"; step_some_test "Returns
   \"Playing\" when stepping camel continues for game_ten"
   (Game.set_dir_game game_ten Up) "Playing"; ] *)

let suite =
  "test suite for Game"
  >::: Stdlib.List.flatten
         [
           true_in_bounds_tests;
           false_in_bounds_tests;
           create_tests;
           set_direction_tests;
         ]

let _ = run_test_tt_main suite