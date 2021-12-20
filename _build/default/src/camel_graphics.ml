open! Base

module Colors = struct
  let white = Graphics.rgb 255 255 255
  let black = Graphics.rgb 000 000 000
  let gray = Graphics.rgb 128 128 128
  let red = Graphics.rgb 255 000 000
  let orange = Graphics.rgb 255 165 000
  let yellow = Graphics.rgb 255 255 000
  let green = Graphics.rgb 000 255 000
  let blue = Graphics.rgb 000 000 255
  let purple = Graphics.rgb 255 000 255
  let sand = Graphics.rgb 255 137 058
  let camel_img = Graphics.rgb 255 235 097
  let cactus_img = Graphics.rgb 050 162 061
  let game_start = Graphics.rgb 255 255 255
  let game_playing = Graphics.rgb 000 000 000
  let game_lost = Graphics.rgb 000 000 000
  let game_won = Graphics.rgb 000 000 000
end

module Constants = struct
  let play_area_height = 400
  let header_height = 50
  let total_height = 450
  let play_area_width = 450
  let block_size = 18
  let block_width = 18
  let block_height = 18
  let block_area = 324
  let header_width = 450
  let total_width = 450
  let total_area = 202500
end

let only_one : bool ref = ref false

let init () =
  let open Constants in
  if !only_one then failwith "You can only call init once."
  else only_one := true;
  Graphics.open_graph
    (Printf.sprintf " %dx%d"
       (play_area_height + header_height)
       play_area_width);
  let height = play_area_height / block_size in
  let width = play_area_width / block_size in
  Game.create ~height ~width ~initial_caravan_length:1

let draw_img { Position.row; col } ~color =
  let open Constants in
  let col = col * block_size in
  let new_col = col + 1 in
  let row = row * block_size in
  let new_row = row + 1 in
  let new_block_size = block_size - 1 in
  Graphics.set_color color;
  Graphics.fill_rect new_col new_row new_block_size new_block_size

let randomize_color c =
  match c with
  | "white" -> Graphics.set_color Colors.white
  | "gray" -> Graphics.set_color Colors.gray
  | "black" -> Graphics.set_color Colors.black
  | "red" -> Graphics.set_color Colors.red
  | "orange" -> Graphics.set_color Colors.orange
  | "yellow" -> Graphics.set_color Colors.yellow
  | "green" -> Graphics.set_color Colors.green
  | "blue" -> Graphics.set_color Colors.blue
  | "purple" -> Graphics.set_color Colors.purple
  | "sand" -> Graphics.set_color Colors.sand
  | _ -> Graphics.set_color Colors.sand

let randomize_text i =
  match i with
  | 1 -> Graphics.set_text_size Constants.total_height
  | 2 -> Graphics.set_text_size Constants.block_width
  | 3 -> Graphics.set_text_size Constants.block_height
  | 4 -> Graphics.set_text_size Constants.block_area
  | 5 -> Graphics.set_text_size Constants.header_width
  | 6 -> Graphics.set_text_size Constants.total_width
  | 7 -> Graphics.set_text_size Constants.total_area
  | _ -> Graphics.set_text_size 20

let draw_header ~game_state c =
  let open Constants in
  let header_color =
    match (game_state : Game_state.t) with
    | Start -> Colors.game_start
    | Playing -> Colors.game_playing
    | Game_over _ -> Colors.game_lost
    | Win -> Colors.game_won
  in
  Graphics.set_color header_color;
  let height = play_area_height in
  let width = play_area_width in
  let header = header_height in
  Graphics.fill_rect 0 height width header;
  let header_text = Game_state.to_string game_state in
  let num_cactus_eaten = Camel.cacti_eaten c in
  let score_text = "Cacti eaten: " ^ Int.to_string num_cactus_eaten in
  randomize_color "sand";
  Graphics.set_color Colors.sand;
  randomize_text 1;
  Graphics.set_text_size 20;
  Graphics.moveto 0 (play_area_height + 25);
  match (game_state : Game_state.t) with
  | Start -> Graphics.draw_string (Printf.sprintf " %s" "")
  | Playing ->
      Graphics.draw_string
        (Printf.sprintf
           " %s                                                        \
            %s"
           header_text score_text)
  | Game_over _ ->
      Graphics.draw_string
        (Printf.sprintf " %s                               %s"
           header_text score_text)
  | Win ->
      let header_color =
        match (game_state : Game_state.t) with
        | Start -> Colors.game_start
        | Playing -> Colors.game_playing
        | Game_over _ -> Colors.game_lost
        | Win -> Colors.game_won
      in
      Graphics.set_color header_color;
      Graphics.draw_string
        (Printf.sprintf " %s                               %s"
           "Congratulations! \n\
           \ As you can see, your caravan has filled the entire game \
            screen. \n\
           \ This means that your hungry camels have eaten all the \
            possible cacti in the entire world so you survived the \
            trek!\n\
           \ " "Thank you so much for playing our game!")

let draw_play_area ~game_state =
  let open Constants in
  let body_text = Game_state.to_string game_state in
  match (game_state : Game_state.t) with
  | Start ->
      randomize_color "sand";
      Graphics.set_text_size 20;
      Graphics.draw_string (Printf.sprintf " %s" body_text);
      let width = play_area_width in
      let height = play_area_height in
      Graphics.fill_rect 0 0 width height;
      Graphics.moveto (play_area_width / 2) (play_area_height / 2)
  | _ ->
      Graphics.set_color Colors.sand;
      let width = play_area_width in
      let height = play_area_height in
      Graphics.fill_rect 0 0 width height

let draw_cactus cactus =
  let cactus_location = Cactus.location cactus in
  let cactus_color = Colors.cactus_img in
  draw_img cactus_location ~color:cactus_color

let draw_camel caravan_locations =
  let camel_color = Colors.camel_img in
  List.iter caravan_locations ~f:(draw_img ~color:camel_color)

let render game =
  let caravan = Game.caravan game in
  let cactus = Game.cactus game in
  let game_state = Game.game_state game in
  let caravan_locations = Camel.locations caravan in
  draw_header ~game_state caravan;
  draw_play_area ~game_state;
  draw_cactus cactus;
  draw_camel caravan_locations;
  Graphics.display_mode true;
  Graphics.synchronize ()

let input () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
