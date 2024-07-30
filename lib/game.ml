open! Async
open! Core
open! Fzf

type t =
  { mutable players : Player.t list
  ; commodities_traded : (Commodity.t, int) Hashtbl.t
  ; open_trades : (int, int * Commodity.t) Hashtbl.t
  ; mutable game_full : bool
  ; game_id : int
  ; mutable game_listeners :
      (int * (Rpcs.Player_game_data.Response.t -> unit Deferred.t)) list
  (* player_id, listener for player *)
  }
[@@deriving sexp_of]

(* let create_game_from_names (list_of_player_names : String.t list) = let
   num_players = List.length list_of_player_names in let players = List.init
   num_players ~f:(fun idx -> Player. { player_id = idx + 1 ; hand = [] ;
   player_name = List.nth_exn list_of_player_names idx }) in (* Get all the
   types of commodities that we are trading and initialize with quantity 9
   for each commodity *) let commodities_traded = Hashtbl.create (module
   Commodity) in let types_of_commodities_traded = Commodity.game_commodities
   num_players in List.iter types_of_commodities_traded ~f:(fun commodity ->
   Hashtbl.set commodities_traded ~key:commodity ~data:9); let open_trades =
   Hashtbl.create (module Int) in { players; commodities_traded; open_trades
   } ;;

   let create_game num_players = (* Number of players is equal to the number
   of commodites traded *) let players = List.init num_players ~f:(fun
   player_id -> Player.{ player_id = player_id + 1; hand = []; player_name =
   "" }) in (* Get all the types of commodities that we are trading and
   initialize with quantity 9 for each commodity *) let commodities_traded =
   Hashtbl.create (module Commodity) in let types_of_commodities_traded =
   Commodity.game_commodities num_players in List.iter
   types_of_commodities_traded ~f:(fun commodity -> Hashtbl.set
   commodities_traded ~key:commodity ~data:9); let open_trades =
   Hashtbl.create (module Int) in { players; commodities_traded; open_trades
   } ;; *)

let get_hand_for_player t target_player_id =
  let target_player =
    List.find_exn t.players ~f:(fun player ->
      Int.equal target_player_id player.player_id)
  in
  target_player.hand
;;

(* cleanup later on *)
(* Caused an error when we used Game_lib.win_check *)
let win_checker (player : Player.t) =
  match equal (List.length player.hand) 0 with
  | true -> false
  | false ->
    let first_commodity = List.nth_exn player.hand 0 in
    List.for_all player.hand ~f:(fun commodity ->
      Commodity.equal first_commodity commodity)
;;

let get_list_of_winning_players t =
  let winners =
    List.filter t.players ~f:(fun player -> win_checker player)
  in
  List.map winners ~f:(fun winning_player ->
    winning_player, List.hd_exn winning_player.hand)
;;

let has_winners t = List.length (get_list_of_winning_players t) > 0

let get_player (game : t) player_id =
  let players_list = game.players in
  let player_match_opt =
    List.find players_list ~f:(fun player ->
      Int.equal player.player_id player_id)
  in
  match player_match_opt with
  | Some player -> player
  | None -> failwith "No player matches given ID"
;;

let change_hand ~(player : Player.t) ~old_commodity ~new_commodity ~num_cards
  =
  let list_of_new_commodity =
    List.init num_cards ~f:(fun _ -> new_commodity)
  in
  let hand_without_old_commodity =
    List.filter player.hand ~f:(fun player_commodity ->
      not (Commodity.equal player_commodity old_commodity))
  in
  let correct_amount_of_old_commodity =
    9 - List.length (list_of_new_commodity @ hand_without_old_commodity)
  in
  let correct_old_commodity_list =
    List.init correct_amount_of_old_commodity ~f:(fun _ -> old_commodity)
  in
  print_s
    [%message
      ""
        (hand_without_old_commodity : Commodity.t list)
        (correct_old_commodity_list : Commodity.t list)];
  player.hand
  <- List.sort
       ~compare:Commodity.compare
       (hand_without_old_commodity
        @ list_of_new_commodity
        @ correct_old_commodity_list)
;;

let handle_trade (game : t) (player : Player.t) commodity_to_trade num_cards =
  let player_hand = player.hand in
  let num_of_commodity =
    List.length
      (List.filter player_hand ~f:(fun commodity ->
         Commodity.equal commodity commodity_to_trade))
  in
  if num_of_commodity < num_cards
  then
    (* print_endline "Trade Rejected: Invalid number of cards - must be
       1-4."; *)
    Deferred.return
      (Rpcs.Make_trade.Response.Trade_rejected
         "Invalid number of cards - must be 1-4")
  else if List.mem (Hashtbl.keys game.open_trades) num_cards ~equal:Int.equal
  then (
    let other_player_id, other_commodity =
      Hashtbl.find_exn game.open_trades num_cards
    in
    let other_player = get_player game other_player_id in
    match Player.equal player other_player with
    | true ->
      Core.print_endline "Trade Rejected: Offer already in the book.";
      Deferred.return
        (Rpcs.Make_trade.Response.Trade_rejected
           "Trade Rejected: Offer already in the book.")
    | false ->
      change_hand
        ~player
        ~old_commodity:commodity_to_trade
        ~new_commodity:other_commodity
        ~num_cards;
      change_hand
        ~player:(get_player game other_player_id)
        ~old_commodity:other_commodity
        ~new_commodity:commodity_to_trade
        ~num_cards;
      Core.printf
        "Trade of %d cards successful between player %d and %d \n"
        num_cards
        player.player_id
        other_player_id;
      Hashtbl.remove game.open_trades num_cards;
      Deferred.return
        (Rpcs.Make_trade.Response.Trade_successful
           (player.player_id, other_player_id)))
  else (
    Hashtbl.add_exn
      game.open_trades
      ~key:num_cards
      ~data:(player.player_id, commodity_to_trade);
    Deferred.return Rpcs.Make_trade.Response.In_book)
;;

(* print_endline "No matching trade found - offer placed on book") *)

let generate_player_hands (game : t) =
  List.iter game.players ~f:(fun player ->
    let hand =
      List.init 9 ~f:(fun _ ->
        let commodities_being_traded =
          Hashtbl.keys game.commodities_traded
        in
        let pool_of_commodites =
          List.filter commodities_being_traded ~f:(fun commodity ->
            not (Hashtbl.find_exn game.commodities_traded commodity = 0))
        in
        let chosen_commodity = List.random_element_exn pool_of_commodites in
        let current_num =
          Hashtbl.find_exn game.commodities_traded chosen_commodity
        in
        Hashtbl.set
          game.commodities_traded
          ~key:chosen_commodity
          ~data:(current_num - 1);
        chosen_commodity)
    in
    player.hand <- List.sort ~compare:Commodity.compare hand)
;;

let create_empty_game game_id =
  { players = []
  ; commodities_traded = Hashtbl.create (module Commodity)
  ; open_trades = Hashtbl.create (module Int)
  ; game_full = false
  ; game_id
  ; game_listeners = []
  }
;;

let add_to_game_listeners game player_id listener =
  (* We need to filter out the current listener for the player if it
     exists *)
  let old_game_listeners =
    List.filter game.game_listeners ~f:(fun player_listener_pair ->
      let curr_player_id, _ = player_listener_pair in
      not (equal curr_player_id player_id))
  in
  let new_game_listeners =
    List.append old_game_listeners [ player_id, listener ]
  in
  game.game_listeners <- new_game_listeners
;;

let start_game t =
  let num_players = List.length t.players in
  let types_of_commodities_traded = Commodity.game_commodities num_players in
  List.iter types_of_commodities_traded ~f:(fun commodity ->
    Hashtbl.set t.commodities_traded ~key:commodity ~data:9);
  t.game_full <- true;
  generate_player_hands t
;;

let add_player_to_game t player =
  let new_players_list = List.append t.players [ player ] in
  t.players <- new_players_list
;;
