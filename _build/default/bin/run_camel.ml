open! Base
open! Camel_lib

let every seconds ~f ~stop =
  let open Async in
  let open Core in
  let rec loop () =
    if !stop then return ()
    else
      Clock.after (Time.Span.of_sec seconds) >>= fun () ->
      f ();
      loop ()
  in
  don't_wait_for (loop ())

let handle_keys (game : Game.t) ~game_over =
  every ~stop:game_over 0.01 ~f:(fun () ->
      match Camel_graphics.input () with
      | None -> ()
      | Some key -> (
          let set_direction dir = Game.set_direction game dir in
          match key with
          | 'w' -> set_direction Up
          | 'a' -> set_direction Left
          | 's' -> set_direction Down
          | 'd' -> set_direction Right
          | 'c' -> ()
          | _ -> ()))

let handle_steps (game : Game.t) ~game_over =
  every ~stop:game_over 0.1 ~f:(fun () ->
      Game.step game;
      Camel_graphics.render game;
      match Game.game_state game with
      | Game_over _
      | Win ->
          game_over := true
      | Start
      | Playing ->
          ())

let run () =
  let game = Camel_graphics.init () in
  Camel_graphics.render game;
  let game_over = ref false in
  handle_keys game ~game_over;
  handle_steps game ~game_over

let () =
  run ();
  Core_kernel.never_returns (Async.Scheduler.go ())
