open! Core
open! Async
open! Fzf
open Curses

type t =
  { parent_window : window
  ; height : int
  ; width : int
  ; make_trade_window : window
  ; hand_window : window
  ; book_window : window
  }

let prerr err =
  match err with
  | false ->
    endwin ();
    failwith "Error!"
  | true -> ()
;;

let create_hand_window parent_height parent_width =
  let height = parent_height / 2 in
  let width = parent_width / 2 in
  let window = newwin height width 0 width in
  box window 124 45;
  window
;;

let create_book_window parent_height parent_width =
  let height = parent_height / 2 in
  let width = parent_width / 2 in
  let window = newwin height width height width in
  box window 124 45;
  window
;;

let create_make_trade_window parent_height parent_width =
  let width = parent_width / 2 in
  let window = newwin parent_height width 0 0 in
  box window 124 45;
  window
;;

let refresh_all_windows t =
  wrefresh t.parent_window |> prerr;
  wrefresh t.book_window |> prerr;
  wrefresh t.hand_window |> prerr
;;

let init () : t =
  let parent_window = initscr () in
  (* Turn off input buffering *)
  cbreak () |> prerr;
  refresh () |> prerr;
  let height, width = getmaxyx parent_window in
  let hand_window = create_hand_window height width in
  let book_window = create_book_window height width in
  let make_trade_window = create_make_trade_window height width in
  let ui =
    { parent_window
    ; height
    ; width
    ; make_trade_window
    ; hand_window
    ; book_window
    }
  in
  refresh_all_windows ui;
  ui
;;

let update_hand t (hand : Commodity.t list) =
  werase t.hand_window;
  let updated_hand = Player.hand_to_string hand in
  mvwaddstr t.hand_window 1 1 updated_hand |> prerr;
  refresh_all_windows t
;;

let update_book t book =
  werase t.book_window;
  let updated_book = Game.book_to_string book in
  mvwaddstr t.book_window 1 1 updated_book |> prerr;
  refresh_all_windows t
;;

let update_game_over t message =
  werase t.hand_window;
  werase t.book_window;
  mvwaddstr t.hand_window 1 1 message |> prerr;
  refresh_all_windows t
;;
