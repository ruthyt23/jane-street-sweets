open! Async
open! Core
open! Fzf

type t =
  { players : Player.t list
  ; commodities_traded : (Commodity.t, int) Hashtbl.t
  ; open_trades : (int, int * Commodity.t) Hashtbl.t
  }

let create_game_from_names (list_of_player_names : String.t list) =
  let num_players = List.length list_of_player_names in
  let players =
    List.init num_players ~f:(fun idx ->
      Player.
        { player_id = idx + 1
        ; hand = []
        ; player_name = List.nth_exn list_of_player_names idx
        })
  in
  (* Get all the types of commodities that we are trading and initialize with
     quantity 9 for each commodity *)
  let commodities_traded = Hashtbl.create (module Commodity) in
  let types_of_commodities_traded = Commodity.game_commodities num_players in
  List.iter types_of_commodities_traded ~f:(fun commodity ->
    Hashtbl.set commodities_traded ~key:commodity ~data:9);
  let open_trades = Hashtbl.create (module Int) in
  { players; commodities_traded; open_trades }
;;

let create_game num_players =
  (* Number of players is equal to the number of commodites traded *)
  let players =
    List.init num_players ~f:(fun player_id ->
      Player.{ player_id = player_id + 1; hand = []; player_name = "" })
  in
  (* Get all the types of commodities that we are trading and initialize with
     quantity 9 for each commodity *)
  let commodities_traded = Hashtbl.create (module Commodity) in
  let types_of_commodities_traded = Commodity.game_commodities num_players in
  List.iter types_of_commodities_traded ~f:(fun commodity ->
    Hashtbl.set commodities_traded ~key:commodity ~data:9);
  let open_trades = Hashtbl.create (module Int) in
  { players; commodities_traded; open_trades }
;;

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

let check_for_wins t =
  let winners =
    List.filter t.players ~f:(fun player -> win_checker player)
  in
  List.map winners ~f:(fun winning_player ->
    winning_player, List.hd_exn winning_player.hand)
;;

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
  player.hand
  <- List.sort
       ~compare:Commodity.compare
       (hand_without_old_commodity @ list_of_new_commodity)
;;

let handle_trade (game : t) (player : Player.t) commodity_to_trade num_cards =
  let player_hand = player.hand in
  let num_of_commodity =
    List.length
      (List.filter player_hand ~f:(fun commodity ->
         Commodity.equal commodity commodity_to_trade))
  in
  if num_of_commodity < num_cards || num_cards < 1 || num_cards > 4
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
      Deferred.return Rpcs.Make_trade.Response.Trade_successful)
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
