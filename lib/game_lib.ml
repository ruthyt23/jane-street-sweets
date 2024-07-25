open! Async
open! Core
open! Fzf
module Client = Client
module Server = Server

let get_player (game : Game.t) player_id =
  let players_list = game.players in
  let player_match_opt =
    List.find players_list ~f:(fun player ->
      Int.equal player.player_id player_id)
  in
  match player_match_opt with
  | Some player -> player
  | None -> failwith "No player matches given ID"
;;

let generate_player_hands (game : Game.t) =
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

let handle_trade
  (game : Game.t)
  (player : Player.t)
  commodity_to_trade
  num_cards
  =
  let player_hand = player.hand in
  let num_of_commodity =
    List.length
      (List.filter player_hand ~f:(fun commodity ->
         Commodity.equal commodity commodity_to_trade))
  in
  if num_of_commodity < num_cards || num_cards < 1 || num_cards > 4
  then print_endline "Trade Rejected: Invalid number of cards - must be 1-4."
  else if List.mem
            (Hashtbl.keys game.open_trades)
            num_of_commodity
            ~equal:Int.equal
  then (
    let other_player_id, other_commodity =
      Hashtbl.find_exn game.open_trades num_of_commodity
    in
    let other_player = get_player game other_player_id in
    match Player.equal player other_player with
    | true -> print_endline "Trade Rejected: Offer already in the book."
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
      printf
        "Trade of %d cards successful between player %d and %d \n"
        num_cards
        player.player_id
        other_player_id)
  else (
    Hashtbl.add_exn
      game.open_trades
      ~key:num_cards
      ~data:(player.player_id, commodity_to_trade);
    print_endline "No matching trade found - offer placed on book")
;;

let win_check (player : Player.t) =
  let first_commodity = List.nth_exn player.hand 0 in
  List.for_all player.hand ~f:(fun commodity ->
    Commodity.equal first_commodity commodity)
;;

let _print_hands (game : Game.t) =
  List.iter game.players ~f:(fun player -> Player.print_hand player)
;;
