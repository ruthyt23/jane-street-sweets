open! Core
open! Async
open! Fzf
open Curses

type t =
  { parent_window : window
  ; height : int
  ; width : int
  ; hand_window : window
  ; book_window : window
  }

let create_hand_window parent_height parent_width =
  let height = parent_height / 2 in
  newwin height parent_width 0 0
;;

let create_book_window parent_height parent_width =
  let height = parent_height / 2 in
  newwin height parent_width height 0
;;

let prerr err =
  match err with
  | false ->
    endwin ();
    failwith "Error!"
  | true -> ()
;;

let init () : t =
  let parent_window = initscr () in
  (* Turn off input buffering *)
  cbreak () |> prerr;
  refresh () |> prerr;
  let height, width = getmaxyx parent_window in
  let hand_window = create_hand_window height width in
  let book_window = create_book_window height width in
  { parent_window; height; width; hand_window; book_window }
;;

let update_hand t (hand : Commodity.t list) =
  werase t.hand_window;
  let updated_hand = Player.hand_to_string hand in
  mvwaddstr t.hand_window 1 1 updated_hand |> prerr
;;

let update_book t book =
  werase t.book_window;
  let updated_book = Game.book_to_string book in
  mvwaddstr t.book_window 1 1 updated_book |> prerr
;;
