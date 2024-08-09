open! Core
open! Async
open Curses

let prerr err =
  match err with
  | false ->
    endwin ();
    failwith "Error!"
  | true -> ()
;;

module Input = struct
  type t =
    | Up
    | Down
    | Escape
    | Enter
    | Invalid

  let of_int value =
    match value with
    | 119 -> Up (* arrow: 27 91 65 *)
    | 115 -> Down (* arrow: 27 91 66 *)
    | 27 -> Escape
    | 10 -> Enter
    | _ -> Invalid
  ;;
end

module State_manager = struct
  module Current_state = struct
    type t =
      | Selecting_Commodity
      | Selecting_Quantity
    [@@deriving equal]
  end

  type t =
    { mutable hand_cursor : int
    ; mutable quantity_cursor : int
    ; mutable hand_selection_list : Commodity.t list
    ; quantity_selection_list : int list
    ; mutable state : Current_state.t
    ; mutable curr_commodity_selected : Commodity.t option
    ; mutable curr_quantity_selected : int option
    }

  let init () =
    { hand_cursor = 0
    ; quantity_cursor = 0
    ; hand_selection_list = []
    ; quantity_selection_list = [ 1; 2; 3; 4 ]
    ; state = Selecting_Commodity
    ; curr_commodity_selected = None
    ; curr_quantity_selected = None
    }
  ;;

  let reset t =
    t.state <- Selecting_Commodity;
    t.quantity_cursor <- 0;
    t.hand_cursor <- 0;
    t.curr_commodity_selected <- None;
    t.curr_quantity_selected <- None
  ;;

  let move_up t =
    match t.state with
    | Selecting_Commodity ->
      t.hand_cursor
      <- (t.hand_cursor - 1) % List.length t.hand_selection_list
    | Selecting_Quantity -> t.quantity_cursor <- (t.quantity_cursor - 1) % 4
  ;;

  let move_down t =
    match t.state with
    | Selecting_Commodity ->
      t.hand_cursor
      <- (t.hand_cursor + 1) % List.length t.hand_selection_list
    | Selecting_Quantity -> t.quantity_cursor <- (t.quantity_cursor + 1) % 4
  ;;

  let handle_enter t =
    match t.state with
    | Selecting_Commodity ->
      t.curr_commodity_selected
      <- List.nth t.hand_selection_list t.hand_cursor;
      t.state <- Selecting_Quantity
    | Selecting_Quantity ->
      t.curr_quantity_selected
      <- List.nth t.quantity_selection_list t.quantity_cursor
  ;;

  let handle_escape t = reset t
end

type t =
  { parent_window : window
  ; height : int
  ; width : int
  ; trade_parent_window : window
  ; commodity_window : window
  ; quantity_window : window
  ; hand_window : window
  ; book_update_window : window
  ; trade_update_window : window
  ; state_manager : State_manager.t
  }

let acs_codes = get_acs_codes ()
let nice_box window = box window acs_codes.vline acs_codes.hline
let clear window = wclear window

let newline_addstr window message =
  waddstr window message |> prerr;
  waddstr window "\n" |> prerr
;;

let heading ?(y = 0) ?(x = 0) ~window ~text () =
  wattr_on window (A.color_pair 1);
  if y = 0 && x = 0
  then newline_addstr window text
  else mvwaddstr window y x text |> prerr;
  wattr_off window (A.color_pair 1)
;;

let create_book_update_window parent_height parent_width =
  let height = parent_height / 2 in
  let width = parent_width / 2 in
  let window = newwin height width 0 0 in
  nice_box window;
  wrefresh window |> prerr;
  let res = derwin window (height - 2) (width - 2) 1 1 in
  scrollok res true;
  res
;;

let create_trade_update_window parent_height parent_width =
  let height = parent_height / 2 in
  let width = parent_width / 2 in
  let window = newwin (height + 1) width height 0 in
  nice_box window;
  wrefresh window |> prerr;
  let res = derwin window (height - 2) (width - 2) 1 1 in
  scrollok res true;
  res
;;

let create_hand_window parent_height parent_width =
  let height = parent_height / 2 in
  let width = parent_width / 2 in
  let window = newwin height width 0 width in
  nice_box window;
  wrefresh window |> prerr;
  derwin window (height - 2) (width - 2) 1 1
;;

let create_make_trade_window parent_height parent_width =
  let height = (parent_height / 2) + 1 in
  let width = parent_width / 2 in
  let parent_window = newwin height width (height - 1) width in
  let small_height = height - 3 in
  let small_width = (width - 2) / 2 in
  let commodity_window = derwin parent_window small_height small_width 2 1 in
  let quantity_window =
    derwin parent_window small_height small_width 2 (width / 2)
  in
  nice_box parent_window;
  nice_box commodity_window;
  nice_box quantity_window;
  let derived_commodity_window =
    derwin commodity_window (small_height - 2) (small_width - 2) 1 1
  in
  let derived_quantity_window =
    derwin quantity_window (small_height - 2) (small_width - 2) 1 1
  in
  heading
    ~window:parent_window
    ~text:"Select the trade you want to make:"
    ~y:1
    ~x:1
    ();
  parent_window, derived_commodity_window, derived_quantity_window
;;

let reset_commodity_window t =
  let y, x = ref 1, 1 in
  let state_manager, commodity_window =
    t.state_manager, t.commodity_window
  in
  List.iteri state_manager.hand_selection_list ~f:(fun index commodity ->
    if State_manager.Current_state.equal
         state_manager.state
         Selecting_Commodity
       && index = state_manager.hand_cursor
    then wattron commodity_window Curses.A.standout
    else wattroff commodity_window Curses.A.standout;
    mvwaddstr commodity_window !y x (Commodity.to_string commodity) |> prerr;
    y := !y + 1);
  wrefresh commodity_window |> prerr
;;

let reset_quantity_window t =
  let y, x = ref 1, 1 in
  let state_manager, quantity_window = t.state_manager, t.quantity_window in
  List.iteri state_manager.quantity_selection_list ~f:(fun index quantity ->
    if State_manager.Current_state.equal
         state_manager.state
         Selecting_Quantity
       && index = state_manager.quantity_cursor
    then wattron quantity_window Curses.A.standout
    else wattroff quantity_window Curses.A.standout;
    (* wbkgdset quantity_window 73; *)
    (* y := 20; *)
    mvwaddstr quantity_window !y x (Int.to_string quantity) |> prerr;
    y := !y + 1);
  wrefresh quantity_window |> prerr
;;

let reset_hand_window window =
  clear window;
  heading ~window ~text:"Player Hand:" ()
;;

let reset_book_update_window window =
  clear window;
  heading ~window ~text:"Book:" ()
;;

let reset_trade_update_window window =
  clear window;
  heading ~window ~text:"Updates:" ()
;;

let refresh_all_windows t =
  wrefresh t.parent_window |> prerr;
  wrefresh t.trade_parent_window |> prerr;
  wrefresh t.commodity_window |> prerr;
  wrefresh t.quantity_window |> prerr;
  wrefresh t.book_update_window |> prerr;
  wrefresh t.trade_update_window |> prerr;
  wrefresh t.hand_window |> prerr
;;

let init () : t =
  let parent_window = initscr () in
  (* Turn off input buffering *)
  cbreak () |> prerr;
  refresh () |> prerr;
  let height, width = getmaxyx parent_window in
  let state_manager = State_manager.init () in
  let book_update_window = create_book_update_window height width in
  let trade_update_window = create_trade_update_window height width in
  let hand_window = create_hand_window height width in
  let trade_parent_window, commodity_window, quantity_window =
    create_make_trade_window height width
  in
  let ui =
    { parent_window
    ; height
    ; width
    ; trade_parent_window
    ; commodity_window
    ; quantity_window
    ; hand_window
    ; book_update_window
    ; trade_update_window
    ; state_manager
    }
  in
  start_color () |> prerr;
  init_pair 1 Color.red Color.white |> prerr;
  reset_book_update_window book_update_window;
  reset_trade_update_window trade_update_window;
  reset_commodity_window ui;
  reset_quantity_window ui;
  refresh_all_windows ui;
  ui
;;

let update_hand t (hand : Commodity.t list) =
  reset_hand_window t.hand_window;
  let updated_hand = Player.hand_to_string hand in
  let hand_selection_list =
    List.dedup_and_sort hand ~compare:Commodity.compare
  in
  t.state_manager.hand_selection_list <- hand_selection_list;
  State_manager.reset t.state_manager;
  mvwaddstr t.hand_window 2 1 updated_hand |> prerr;
  clear t.commodity_window;
  clear t.quantity_window;
  reset_commodity_window t;
  reset_quantity_window t
;;

let update_book t book player_id =
  reset_book_update_window t.book_update_window;
  let updated_book = Game.book_to_string book player_id in
  newline_addstr t.book_update_window updated_book;
  wrefresh t.book_update_window |> prerr
;;

let update_game_over t message =
  werase t.hand_window;
  werase t.book_update_window;
  werase t.trade_update_window;
  mvwaddstr t.hand_window 1 1 message |> prerr;
  refresh_all_windows t
;;

let display_trade_update t message start =
  if not start then ();
  reset_trade_update_window t.trade_update_window;
  newline_addstr t.trade_update_window message
;;

let manage_user_input t =
  timeout 0;
  noecho () |> prerr;
  curs_set 0 |> prerr;
  State_manager.reset t.state_manager;
  reset_commodity_window t;
  reset_quantity_window t;
  Deferred.repeat_until_finished () (fun () ->
    (match t.state_manager.state with
     | State_manager.Current_state.Selecting_Commodity ->
       reset_commodity_window t
     | State_manager.Current_state.Selecting_Quantity ->
       reset_quantity_window t);
    refresh_all_windows t;
    let user_input = getch () in
    match Input.of_int user_input with
    | Up ->
      State_manager.move_up t.state_manager;
      return (`Repeat ())
    | Down ->
      State_manager.move_down t.state_manager;
      return (`Repeat ())
    | Escape ->
      State_manager.handle_escape t.state_manager;
      reset_commodity_window t;
      reset_quantity_window t;
      return (`Repeat ())
    | Enter ->
      State_manager.handle_enter t.state_manager;
      (match
         ( t.state_manager.curr_commodity_selected
         , t.state_manager.curr_quantity_selected )
       with
       | Some commodity, Some quantity ->
         return (`Finished (commodity, quantity))
       | _ -> return (`Repeat ()))
    | Invalid -> return (`Repeat ()))
;;
