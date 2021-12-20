open! Base

type t =
  | Start
  | Playing
  | Game_over of string
  | Win
[@@deriving sexp_of]

let to_string t =
  match t with
  | Start ->
      "Welcome to Camel (Eat That Cactus!) \n\
      \ The objective of this game is for your caravan of camels to \
       eat the cactus that is displayed on the screen. In order to \
       move your carvan, press the keys W,A,S,D in order to move your \
       caravan up, left, down, and right, respectively. \n\
      \ You win when the caravan fills the entire screen. \n\
      \ Good luck and happy playing! \n\
      \ \n\
      \ Press 'C' for Camel to START!"
  | Playing -> ""
  | Game_over x -> "Game over: " ^ x
  | Win ->
      "Congratulations! \n\
      \ You have just won our... \n\
      \ CS 3110 FINAL PROJECT CAMEL GAME! \n\
      \ If you want to learn more about how we created this game, feel \
       free to reach out to us at: \n\
      \ lz382@cornell.edu \n\
      \ and \n\
      \ mwy9@cornell.edu \n\
      \ and \n\
      \ jlc28@cornell.edu \n\
      \ for more information.\n\
      \      \n\
      \ \n\
      \ \n\
      \ We now proclaim \n\
      \ that you are the \n\
      \ BEST \n\
      \ CAMEL \n\
      \ GAME \n\
      \ PLAYER \n\
      \ in the \n\
      \ Whole. Entire. Universe! \n\
      \ You are now equipped to traverse the Sahara Desert all \n\
      \ (or any desert in the world) \n\
      \ by yourself!"
