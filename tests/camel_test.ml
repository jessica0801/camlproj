open! Base
open! Camel_lib
open OUnit2
open Camel

(* test orientation *)
let orientation_test
    (name : string)
    (c : Camel.caravan)
    (expected_output : Camel.direction) : test =
  name >:: fun _ -> assert_equal expected_output (orientation c)

(* test camels_left *)
let camels_left_test
    (name : string)
    (c : Camel.caravan)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (camels_left c)

(* test number of cacti eaten *)
(*increases by 1 everytime camels left increases by one*)
let cacti_eaten_test
    (name : string)
    (c : Camel.caravan)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (cacti_eaten c)

(* test locations *)
let locations_test
    (name : string)
    (c : Camel.caravan)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (List.length (locations c))

(* test create *)

let zero_camels = create 0
let one_camel = create 1
let two_camels = create 2
let three_camels = create 3
let four_camels = create 4
let five_camels = create 5
let eight_camels = create 8
let nine_camels = create 9
let ten_camels = create 10

let create_tests =
  [
    (* zero camels *)
    orientation_test "Orientation of zero camels is Right" zero_camels
      Right;
    camels_left_test "Camels left of zero camels is zero" zero_camels 0;
    locations_test "List of locations of zero camels is length zero"
      zero_camels 0;
    (* one camel *)
    orientation_test "Orientation of one camel is Right" one_camel Right;
    camels_left_test "Camels left of one camel is zero" one_camel 0;
    locations_test "List of locations of one camel is length one"
      one_camel 1;
    (* two camels *)
    orientation_test "Orientation of two camels is Right" two_camels
      Right;
    camels_left_test "Camels left of two camels is zero" two_camels 0;
    locations_test "List of locations of two camels is length two"
      two_camels 2;
    (* three camels *)
    orientation_test "Orientation of three camels is Right" three_camels
      Right;
    camels_left_test "Camels left of three camels is zero" three_camels
      0;
    locations_test "List of locations of three camels is length three"
      three_camels 3;
    (* five camels *)
    orientation_test "Orientation of five camels is Right" five_camels
      Right;
    camels_left_test "Camels left of five camels is zero" five_camels 0;
    locations_test "List of locations of five camels is length five"
      five_camels 5;
    (* eight camels *)
    orientation_test "Orientation of eight camels is Right" eight_camels
      Right;
    camels_left_test "Camels left of eight camels is zero" eight_camels
      0;
    locations_test "List of locations of eight camels is length eight"
      eight_camels 8;
    (* nine camels*)
    orientation_test "Orientation of nine camels is Right" nine_camels
      Right;
    camels_left_test "Camels left of nine camels is zero" nine_camels 0;
    locations_test "List of locations of nine camels is length nine"
      nine_camels 9;
    (* ten camels *)
    orientation_test "Orientation of ten camels is Right" ten_camels
      Right;
    camels_left_test "Camels left of ten camels is zero" ten_camels 0;
    locations_test "List of locations of ten camels is length ten"
      ten_camels 10;
  ]

(* test add_camel *)
(* add_camel_zero creates 0 length caravan with one camel left  *)
let add_camel_zero = add_camel (create 0)
let add_camel_one = add_camel add_camel_zero
let add_camel_two = add_camel add_camel_one
let add_camel_three = add_camel add_camel_two
let add_camel_four = add_camel add_camel_three
let add_camel_five = add_camel add_camel_four

(* caravan of length 1 with 1 camel left to add *)
let add_camel_01 = add_camel (create 1)

(* caravan of length 1 with 2 camels left to add *)
let add_camel_02 = add_camel add_camel_01

(* add_camel_03 is 1 camel with three camels left *)
let add_camel_03 = add_camel add_camel_02
let add_camel_04 = add_camel add_camel_03
let add_camel_05 = add_camel add_camel_04

let add_camel_tests =
  [
    (* add to 0 camels_left for locations length = 0 *)
    orientation_test "Orientation of adding to zero camels is Right"
      add_camel_zero Right;
    camels_left_test "Camels left of adding to zero camels is one"
      add_camel_zero 1;
    cacti_eaten_test "Number of cacti eaten by zero camels is one"
      add_camel_zero 1;
    locations_test
      "List of locations of adding to zero camels is length zero"
      add_camel_zero 0;
    (* add to 1 camels_left for locations length = 0 *)
    orientation_test "Orientation of adding to one camel is Right"
      add_camel_one Right;
    camels_left_test "Camels left of adding to one camel is two"
      add_camel_one 2;
    cacti_eaten_test "Number of cacti eaten by one camels is two"
      add_camel_one 2;
    locations_test
      "List of locations of adding to one camel is length zero"
      add_camel_one 0;
    (* add to 2 camels_left for locations length = 0 *)
    orientation_test "Orientation of adding to two camels is Right"
      add_camel_two Right;
    camels_left_test "Camels left of adding to two camels is three"
      add_camel_two 3;
    cacti_eaten_test "Number of cacti eaten by two camels is three"
      add_camel_two 3;
    locations_test
      "List of locations of adding to two camels is length zero"
      add_camel_two 0;
    (* add to 3 camels_left for locations length = 0 *)
    orientation_test
      "Orientation of adding to caravan with three camels left is Right"
      add_camel_three Right;
    camels_left_test
      "Camels left of adding to caravan with three camels left is four"
      add_camel_three 4;
    cacti_eaten_test
      "Number of cacti eaten by caravan with three camels left is four"
      add_camel_three 4;
    locations_test
      "List of locations of adding to caravan with three camels left \
       is length zero"
      add_camel_three 0;
    (* add to 4 camels_left for locations length = 0 *)
    orientation_test
      "Orientation of adding to caravan with four camels left is Right"
      add_camel_four Right;
    camels_left_test
      "Camels left of adding to caravan with four camels left is five"
      add_camel_four 5;
    cacti_eaten_test
      "Number of cacti eaten by caravan with four camels left is five"
      add_camel_four 5;
    locations_test
      "List of locations of adding to caravan with four camels left is \
       length zero"
      add_camel_four 0;
    (* add to 5 camels_left for locations length = 0 *)
    orientation_test
      "Orientation of adding to caravan with five camels left is Right"
      add_camel_five Right;
    camels_left_test
      "Camels left of adding to caravan with five camels left is six"
      add_camel_five 6;
    cacti_eaten_test
      "Number of cacti eaten by caravan with five camels left is six"
      add_camel_five 6;
    locations_test
      "List of locations of adding to caravan with five camels is \
       length zero"
      add_camel_five 0;
    (* add to one camel left with locations = 1 *)
    orientation_test
      "Orientation of adding to zero camels left is Right" add_camel_01
      Right;
    camels_left_test
      "Camels left of adding to zero camels left for camel w/ \
       locations length = 1"
      add_camel_01 1;
    cacti_eaten_test "Number of cacti eaten by zero camels left is one"
      add_camel_01 1;
    locations_test
      "List of locations of adding to zero camels left with locations \
       length = 1"
      add_camel_01 1;
    (* add to two camels left with caravan locations = 1 *)
    orientation_test "Orientation of adding to one camel left is Right"
      add_camel_02 Right;
    camels_left_test
      "Camels left of adding to one camel left for camel w/ locations \
       length = 1"
      add_camel_02 2;
    cacti_eaten_test "Number of cacti eaten by one camels left is two"
      add_camel_02 2;
    locations_test
      "List of locations of adding to one camel left with locations \
       length = 1"
      add_camel_02 1;
    (* add to three camels left with locations = 1 *)
    orientation_test "Orientation of adding to two camels left is Right"
      add_camel_03 Right;
    camels_left_test
      "Camels left of adding to two camels left for camel w/ locations \
       length = 1"
      add_camel_03 3;
    cacti_eaten_test "Number of cacti eaten by two camels left is three"
      add_camel_03 3;
    locations_test
      "List of locations of adding to two camels left with locations \
       length = 1"
      add_camel_03 1;
    (* add to four camels left with locations = 1 *)
    orientation_test
      "Orientation of adding to three camels left is Right" add_camel_04
      Right;
    camels_left_test
      "Camels left of adding to three camels left for camel w/ \
       locations length = 1"
      add_camel_04 4;
    cacti_eaten_test
      "Number of cacti eaten by three camels left is four" add_camel_04
      4;
    locations_test
      "List of locations of adding to three camels left with locations \
       length = 1"
      add_camel_04 1;
    (* add to five camels left with locations = 1 *)
    orientation_test
      "Orientation of adding to four camels left is Right" add_camel_05
      Right;
    camels_left_test
      "Camels left of adding to four camels left for camel w/ \
       locations length = 1"
      add_camel_05 5;
    cacti_eaten_test "Cacti eaten of add to four camels left equals 5"
      add_camel_05 5;
    locations_test
      "List of locations of adding to four camels left with locations \
       length = 1"
      add_camel_05 1;
  ]

(* test first_camel *)

let first_camel_test
    (name : string)
    (c : Camel.caravan)
    (expected_output : Position.t) : test =
  name >:: fun _ -> assert_equal expected_output (first_camel c)

let first_camel_exn_test
    (name : string)
    (c : Camel.caravan)
    (expected_exn : exn) : test =
  name >:: fun _ -> assert_raises expected_exn (fun () -> first_camel c)

let first_camel_tests =
  [
    first_camel_exn_test
      "First camel for camel locations of length 0 raises Failure hd"
      zero_camels (Failure "hd");
    first_camel_test
      "First camel for camel locations of length one has position (0,0)"
      one_camel { row = 0; col = 0 };
    first_camel_test
      "First camel for camel locations of length two has position (0,1)"
      two_camels { row = 0; col = 1 };
    first_camel_test
      "First camel for camel locations of length three has position \
       (0,2)"
      three_camels { row = 0; col = 2 };
    first_camel_test
      "First camel for camel locations of length four has position \
       (0,3)"
      four_camels { row = 0; col = 3 };
    first_camel_test
      "First camel for camel locations of length five has position \
       (0,4)"
      five_camels { row = 0; col = 4 };
    first_camel_test
      "First camel for camel locations of length eight has position \
       (0,0)"
      eight_camels { row = 0; col = 7 };
    first_camel_test
      "First camel for camel locations of length nine has position \
       (0,0)"
      nine_camels { row = 0; col = 8 };
    first_camel_test
      "First camel for camel locations of length 10 has position (0,9)"
      ten_camels { row = 0; col = 9 };
  ]

(* test locations *)

let locations_test
    (name : string)
    (c : Camel.caravan)
    (expected_output : Position.t list) : test =
  name >:: fun _ -> assert_equal expected_output (locations c)

let one_camel_loc : Position.t list = [ { row = 0; col = 0 } ]

let two_camel_loc : Position.t list =
  [ { row = 0; col = 1 }; { row = 0; col = 0 } ]

let three_camel_loc : Position.t list =
  [ { row = 0; col = 2 }; { row = 0; col = 1 }; { row = 0; col = 0 } ]

let four_camel_loc : Position.t list =
  [
    { row = 0; col = 3 };
    { row = 0; col = 2 };
    { row = 0; col = 1 };
    { row = 0; col = 0 };
  ]

let five_camel_loc : Position.t list =
  [
    { row = 0; col = 4 };
    { row = 0; col = 3 };
    { row = 0; col = 2 };
    { row = 0; col = 1 };
    { row = 0; col = 0 };
  ]

let eight_camel_loc : Position.t list =
  [
    { row = 0; col = 7 };
    { row = 0; col = 6 };
    { row = 0; col = 5 };
    { row = 0; col = 4 };
    { row = 0; col = 3 };
    { row = 0; col = 2 };
    { row = 0; col = 1 };
    { row = 0; col = 0 };
  ]

let nine_camel_loc : Position.t list =
  [
    { row = 0; col = 8 };
    { row = 0; col = 7 };
    { row = 0; col = 6 };
    { row = 0; col = 5 };
    { row = 0; col = 4 };
    { row = 0; col = 3 };
    { row = 0; col = 2 };
    { row = 0; col = 1 };
    { row = 0; col = 0 };
  ]

let ten_camel_loc : Position.t list =
  [
    { row = 0; col = 9 };
    { row = 0; col = 8 };
    { row = 0; col = 7 };
    { row = 0; col = 6 };
    { row = 0; col = 5 };
    { row = 0; col = 4 };
    { row = 0; col = 3 };
    { row = 0; col = 2 };
    { row = 0; col = 1 };
    { row = 0; col = 0 };
  ]

let locations_tests =
  [
    locations_test "Locations of a caravan of one camel" one_camel
      one_camel_loc;
    locations_test "Locations of a caravan of two camels" two_camels
      two_camel_loc;
    locations_test "Locations of a caravan of three camels" three_camels
      three_camel_loc;
    locations_test "Locations of a caravan of four camels" four_camels
      four_camel_loc;
    locations_test "Locations of a caravan of five camels" five_camels
      five_camel_loc;
    locations_test "Locations of a caravan of eight camels" eight_camels
      eight_camel_loc;
    locations_test "Locations of a caravan of nine camels" nine_camels
      nine_camel_loc;
    locations_test "Locations of a caravan of ten camels" ten_camels
      ten_camel_loc;
  ]

(* test set_direction *)

let set_direction_test
    (name : string)
    (c : Camel.caravan)
    (d : Camel.direction)
    (expected_output : Camel.direction) : test =
  name >:: fun _ ->
  assert_equal expected_output (orientation (set_direction c d))

let set_direction_tests =
  [
    (* one camel *)
    set_direction_test "Set direction of caravan of one camel to Right"
      one_camel Right Right;
    camels_left_test
      "Camels left for one camel is 0 after set new direction to Right"
      (set_direction one_camel Right)
      0;
    set_direction_test "Set direction of caravan of one camel to Left"
      one_camel Left Left;
    camels_left_test
      "Camels left for one camel is 0 after set new direction to Left"
      (set_direction one_camel Left)
      0;
    set_direction_test "Set direction of caravan of one camel to Up"
      one_camel Up Up;
    camels_left_test
      "Camels left for one camel is 0 after set new direction to Up"
      (set_direction one_camel Up)
      0;
    set_direction_test "Set direction of caravan of one camel to Down"
      one_camel Down Down;
    camels_left_test
      "Camels left for one camel is 0 after set new direction to Down"
      (set_direction one_camel Down)
      0;
    (* two camels *)
    set_direction_test "Set direction of caravan of two camels to Right"
      two_camels Right Right;
    camels_left_test
      "Camels left for two camels is 0 after set new direction to Right"
      (set_direction two_camels Right)
      0;
    set_direction_test "Set direction of caravan of two camels to Left"
      two_camels Left Left;
    camels_left_test
      "Camels left for two camels is 0 after set new direction to Left"
      (set_direction two_camels Left)
      0;
    set_direction_test "Set direction of caravan of two camels to Up"
      two_camels Up Up;
    camels_left_test
      "Camels left for two camels is 0 after set new direction to Up"
      (set_direction two_camels Up)
      0;
    set_direction_test "Set direction of caravan of two camels to Down"
      two_camels Down Down;
    camels_left_test
      "Camels left for two camels is 0 after set new direction to Down"
      (set_direction two_camels Down)
      0;
    (* three camels *)
    set_direction_test
      "Set direction of caravan of three camels to Right" three_camels
      Right Right;
    camels_left_test
      "Camels left for three camel is 0 after set new direction to \
       Right"
      (set_direction three_camels Right)
      0;
    set_direction_test
      "Set direction of caravan of three camels to Left" three_camels
      Left Left;
    camels_left_test
      "Camels left for three camels is 0 after set new direction to \
       Left"
      (set_direction three_camels Left)
      0;
    set_direction_test "Set direction of caravan of three camels to Up"
      three_camels Up Up;
    camels_left_test
      "Camels left for three camels is 0 after set new direction to Up"
      (set_direction three_camels Up)
      0;
    set_direction_test
      "Set direction of caravan of three camels to Down" three_camels
      Down Down;
    camels_left_test
      "Camels left for three camels is 0 after set new direction to \
       Down"
      (set_direction three_camels Down)
      0;
    (* four camels *)
    set_direction_test
      "Set direction of caravan of four camels to Right" four_camels
      Right Right;
    camels_left_test
      "Camels left for four camel is 0 after set new direction to Right"
      (set_direction four_camels Right)
      0;
    set_direction_test "Set direction of caravan of four camels to Left"
      four_camels Left Left;
    camels_left_test
      "Camels left for four camels is 0 after set new direction to Left"
      (set_direction four_camels Left)
      0;
    set_direction_test "Set direction of caravan of four camels to Up"
      four_camels Up Up;
    camels_left_test
      "Camels left for four camels is 0 after set new direction to Up"
      (set_direction four_camels Up)
      0;
    set_direction_test "Set direction of caravan of four camels to Down"
      four_camels Down Down;
    camels_left_test
      "Camels left for four camels is 0 after set new direction to Down"
      (set_direction four_camels Down)
      0;
    (* five camels *)
    set_direction_test
      "Set direction of caravan of five camels to Right" five_camels
      Right Right;
    camels_left_test
      "Camels left for five camel is 0 after set new direction to Right"
      (set_direction five_camels Right)
      0;
    set_direction_test "Set direction of caravan of five camels to Left"
      five_camels Left Left;
    camels_left_test
      "Camels left for five camels is 0 after set new direction to Left"
      (set_direction five_camels Left)
      0;
    set_direction_test "Set direction of caravan of five camels to Up"
      five_camels Up Up;
    camels_left_test
      "Camels left for five camels is 0 after set new direction to Up"
      (set_direction five_camels Up)
      0;
    set_direction_test "Set direction of caravan of five camels to Down"
      five_camels Down Down;
    camels_left_test
      "Camels left for five camels is 0 after set new direction to Down"
      (set_direction five_camels Down)
      0;
    (* eight camels *)
    set_direction_test
      "Set direction of caravan of eight camels to Right" eight_camels
      Right Right;
    camels_left_test
      "Camels left for eight camels is 0 after set new direction to \
       Right"
      (set_direction eight_camels Right)
      0;
    set_direction_test
      "Set direction of caravan of eight camels to Left" eight_camels
      Left Left;
    camels_left_test
      "Camels left for eight camels is 0 after set new direction to \
       Left"
      (set_direction eight_camels Left)
      0;
    set_direction_test "Set direction of caravan of eight camels to Up"
      eight_camels Up Up;
    camels_left_test
      "Camels left for eight camels is 0 after set new direction to Up"
      (set_direction eight_camels Up)
      0;
    set_direction_test
      "Set direction of caravan of eight camels to Down" eight_camels
      Down Down;
    camels_left_test
      "Camels left for eight camels is 0 after set new direction to \
       Down"
      (set_direction eight_camels Down)
      0;
    (* nine camels *)
    set_direction_test
      "Set direction of caravan of nine camels to Right" nine_camels
      Right Right;
    camels_left_test
      "Camels left for nine camel is 0 after set new direction to Right"
      (set_direction nine_camels Right)
      0;
    set_direction_test "Set direction of caravan of nine camels to Left"
      nine_camels Left Left;
    camels_left_test
      "Camels left for nine camels is 0 after set new direction to Left"
      (set_direction nine_camels Left)
      0;
    set_direction_test "Set direction of caravan of nine camels to Up"
      nine_camels Up Up;
    camels_left_test
      "Camels left for nine camels is 0 after set new direction to Up"
      (set_direction nine_camels Up)
      0;
    set_direction_test "Set direction of caravan of nine camels to Down"
      nine_camels Down Down;
    camels_left_test
      "Camels left for nine camels is 0 after set new direction to Down"
      (set_direction nine_camels Down)
      0;
    (* ten camels *)
    set_direction_test "Set direction of caravan of ten camels to Right"
      ten_camels Right Right;
    camels_left_test
      "Camels left for ten camel is 0 after set new direction to Right"
      (set_direction ten_camels Right)
      0;
    set_direction_test "Set direction of caravan of ten camels to Left"
      ten_camels Left Left;
    camels_left_test
      "Camels left for ten camels is 0 after set new direction to Left"
      (set_direction ten_camels Left)
      0;
    set_direction_test "Set direction of caravan of ten camels to Up"
      ten_camels Up Up;
    camels_left_test
      "Camels left for ten camels is 0 after set new direction to Up"
      (set_direction ten_camels Up)
      0;
    set_direction_test "Set direction of caravan of ten camels to Down"
      ten_camels Down Down;
    camels_left_test
      "Camels left for ten camels is 0 after set new direction to Down"
      (set_direction ten_camels Down)
      0;
  ]

(* test next_position *)

let next_position_test
    (name : string)
    (d : Camel.direction)
    (p : Position.t)
    (expected_output : Position.t) : test =
  name >:: fun _ -> assert_equal expected_output (next_position d p)

let next_position_tests =
  [
    (* starting at row = 0, col = 0 *)
    next_position_test
      "Next position for position (0,0) and direction Right is \
       position (0,1)"
      Right { row = 0; col = 0 } { row = 0; col = 1 };
    next_position_test
      "Next position for position (0,0) and direction Left is position \
       (0,-1)"
      Left { row = 0; col = 0 } { row = 0; col = -1 };
    next_position_test
      "Next position for position (0,0) and direction Up is position \
       (1,0)"
      Up { row = 0; col = 0 } { row = 1; col = 0 };
    next_position_test
      "Next position for position (0,0) and direction Down is position \
       (-1,0)"
      Down { row = 0; col = 0 } { row = -1; col = 0 };
    (* starting at row = 1, col = 1 *)
    next_position_test
      "Next position for position (1,1) and direction Right is \
       position (1,2)"
      Right { row = 1; col = 1 } { row = 1; col = 2 };
    next_position_test
      "Next position for position (1,2) and direction Right is \
       position (1,3)"
      Right { row = 1; col = 2 } { row = 1; col = 3 };
    next_position_test
      "Next position for position (0,0) and direction Left is position \
       (1,0)"
      Left { row = 1; col = 1 } { row = 1; col = 0 };
    next_position_test
      "Next position for position (0,0) and direction Up is position \
       (2,1)"
      Up { row = 1; col = 1 } { row = 2; col = 1 };
    next_position_test
      "Next position for position (1,1) and direction Down is position \
       (0,1)"
      Down { row = 1; col = 1 } { row = 0; col = 1 }
    (* next_position_test "Next position for position (2,1) and
       direction Down is position \ (1,1)" Down { row = 2; col = 1 } {
       row = 1; col = 1 }; *);
  ]

(* test step *)

let step_none_test
    (name : string)
    (c : Camel.caravan)
    (expected_output : Camel.caravan option) : test =
  name >:: fun _ -> assert_equal expected_output (step c)

let step_some_test
    (name : string)
    (c : Camel.caravan)
    (expected_len : int)
    (expd_cmls_lft : int)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (match step c with
    | None -> false
    | Some caravan ->
        if
          expected_len = List.length (locations caravan)
          && expd_cmls_lft = camels_left caravan
        then true
        else false)

(* left-facing three camel caravan with positions head=(0,0); (0,1);
   (0,2) *)
let left_loc : Position.t list =
  [ { row = 0; col = 0 }; { row = 0; col = 1 }; { row = 0; col = 2 } ]

(* up-facing three camel caravan with positions head=(2,0); (1,0);
   (0,0) *)
let up_loc : Position.t list =
  [ { row = 2; col = 0 }; { row = 1; col = 0 }; { row = 0; col = 0 } ]

(* down-facing three camel caravan with positions head=(0,0); (1,0);
   (2,0) *)
let down_loc : Position.t list =
  [ { row = 0; col = 0 }; { row = 1; col = 0 }; { row = 2; col = 0 } ]

(* three_left_camels is caravan with head @ (0,0) and body to the right
   of it*)
let three_left_camels = set_locations three_camels left_loc

(* three_up_camels is caravan with head @ (1,1) and body below it*)
let three_up_camels = set_locations three_camels up_loc

(* three_up_camels is caravan with head @ (0,0) and body above it*)
let three_down_camels = set_locations three_camels down_loc

let step_tests =
  [
    step_none_test
      "Returns None when right-facing three camel caravan steps left"
      (set_direction three_camels Left)
      None;
    step_none_test
      "Returns None when left-facing three camel caravan steps right"
      (set_direction three_left_camels Right)
      None;
    step_none_test
      "Returns None when up-facing three camel caravan steps down"
      (set_direction three_up_camels Down)
      None;
    step_none_test
      "Returns None when down-facing three camel caravan steps up"
      (set_direction three_down_camels Up)
      None;
    step_some_test
      "Returns false when right-facing three camel caravan steps left"
      (set_direction three_camels Left)
      3 0 false;
    step_some_test
      "Returns true when right-facing three camel caravan with 0 \
       camels left steps right"
      (set_direction three_camels Right)
      3 0 true;
    (* add_camel_01 currently has one camel left *)
    step_some_test
      "Returns true when right-facing two camel caravan with 1 camel \
       left steps right"
      (set_direction add_camel_01 Right)
      2 0 true;
    (* add_camel_02 currently has 2 camels left *)
    step_some_test
      "Returns true when right-facing two camel caravan with 2 camel \
       left steps right"
      (set_direction add_camel_02 Right)
      2 1 true;
  ]

let suite =
  "test suite for Camel"
  >::: Stdlib.List.flatten
         [
           create_tests;
           add_camel_tests;
           first_camel_tests;
           locations_tests;
           set_direction_tests;
           next_position_tests;
           step_tests;
         ]

let _ = run_test_tt_main suite