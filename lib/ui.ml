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

let acs_codes = get_acs_codes ()
let nice_box window = box window acs_codes.vline acs_codes.hline
let erase_and_box window = wclear window

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
  nice_box window;
  wrefresh window |> prerr;
  derwin window (height - 2) (width - 2) 1 1
;;

let create_book_window parent_height parent_width =
  let width = parent_width / 2 in
  let window = newwin parent_height width 0 0 in
  nice_box window;
  wrefresh window |> prerr;
  derwin window (parent_height - 2) (width - 2) 1 1
;;

let create_make_trade_window parent_height parent_width =
  let height = parent_height / 2 in
  let width = parent_width / 2 in
  let window = newwin height width height width in
  nice_box window;
  mvwaddstr window 1 1 "Input the trade you want to make" |> prerr;
  let input_window = derwin window 3 (width - 2) 3 1 in
  nice_box input_window;
  window
;;

let reset_hand_window window =
  erase_and_box window;
  waddstr window "Player Hand: " |> prerr
;;

let reset_book_window window =
  erase_and_box window;
  waddstr window "Book Updates: " |> prerr
;;

let refresh_all_windows t =
  wrefresh t.parent_window |> prerr;
  wrefresh t.make_trade_window |> prerr;
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
  reset_hand_window t.hand_window;
  let updated_hand = Player.hand_to_string hand in
  mvwaddstr t.hand_window 2 1 updated_hand |> prerr;
  refresh_all_windows t
;;

let update_book t book =
  reset_book_window t.book_window;
  let updated_book = Game.book_to_string book in
  mvwaddstr t.book_window 2 1 updated_book |> prerr;
  refresh_all_windows t
;;

let update_game_over t message =
  werase t.hand_window;
  werase t.book_window;
  mvwaddstr t.hand_window 1 1 message |> prerr;
  refresh_all_windows t
;;

let enable_trade_input t =
  timeout 0;
  ignore (getch ());
  wgetstr t.make_trade_window
;;
