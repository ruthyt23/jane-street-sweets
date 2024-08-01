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
