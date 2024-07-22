open! Core

module Commodity = struct
  type t =
    | Fairlife
    | Pocky
    | Water
  [@@deriving equal, enumerate, compare, sexp_of, hash]

  let all_commodities = all

  (* game_commodities --> generates the n commodities that will be traded in
     the game based on n players *)
  let game_commodities num_players =
    List.init num_players ~f:(fun index ->
      List.nth_exn all_commodities index)
  ;;

  let to_string t =
    match t with
    | Fairlife -> "Fairlife"
    | Pocky -> "Pocky"
    | Water -> "Water"
  ;;

  let of_string str =
    match str with
    | "Fairlife" -> Fairlife
    | "Pocky" -> Pocky
    | "Water" -> Water
    | _ -> failwith "not a valid commodity"
  ;;
end

module Player = struct
  type t =
    { player_id : int
    ; mutable hand : Commodity.t list
    }
  [@@deriving equal]

  (* let update_hand = 0 ;; *)

  let print_hand t =
    Core.printf "Hand for player %d: " t.player_id;
    List.iter t.hand ~f:(fun commodity ->
      Core.printf "%s  " (Commodity.to_string commodity))
  ;;
end

module Game_State = struct
  type t =
    | In_progress
    | Game_over of { winner : Player.t option }
  [@@deriving equal]
end

type t =
  { players : Player.t list
  ; game_state : Game_State.t ref
  ; commodities_traded : (Commodity.t, int) Hashtbl.t
  ; open_trades : (int, int * Commodity.t) Hashtbl.t
  }

let get_player game player_id =
  let players_list = game.players in
  let player_match_opt =
    List.find players_list ~f:(fun player ->
      Int.equal player.player_id player_id)
  in
  match player_match_opt with
  | Some player -> player
  | None -> failwith "No player matches given player_id"
;;

let generate_player_hands t =
  List.iter t.players ~f:(fun player ->
    let hand =
      List.init 9 ~f:(fun _ ->
        let commodities_being_traded = Hashtbl.keys t.commodities_traded in
        let pool_of_commodites =
          List.filter commodities_being_traded ~f:(fun commodity ->
            not (Hashtbl.find_exn t.commodities_traded commodity = 0))
        in
        let chosen_commodity = List.random_element_exn pool_of_commodites in
        let current_num =
          Hashtbl.find_exn t.commodities_traded chosen_commodity
        in
        Hashtbl.set
          t.commodities_traded
          ~key:chosen_commodity
          ~data:(current_num - 1);
        chosen_commodity)
    in
    player.hand <- hand)
;;

(* OLD HANDLE_TRADE - replacing player's card with random cards let
   handle_trade t (player : Player.t) commodity_to_trade num_cards = let
   player_hand = player.hand in let num_of_commodity = List.length
   ((List.filter player_hand ~f:(fun commodity -> Commodity.equal commodity
   commodity_to_trade))) in if not (num_of_commodity = num_cards) then
   Core.print_endline "Trade Rejected: Invalid number of cards." else ( let
   commodities_being_traded = List.filter (Hashtbl.keys t.commodities_traded)
   ~f:(fun commodity -> not (Commodity.equal commodity commodity_to_trade))
   in let new_commodity = List.random_element_exn commodities_being_traded in
   let hand_with_new_commodity = List.init num_cards ~f:(fun _ ->
   new_commodity) in let hand_without_old_commodity = List.filter player_hand
   ~f:(fun player_commodity -> not (Commodity.equal player_commodity
   commodity_to_trade)) in

   player.hand <- (hand_without_old_commodity @ hand_with_new_commodity);
   Core.print_endline "Trade successful! New player hand: ";
   Player.print_hand player; ) ;; *)

let change_hand ~(player : Player.t) ~old_commodity ~new_commodity ~num_cards
  =
  let list_of_new_commodity =
    List.init num_cards ~f:(fun _ -> new_commodity)
  in
  let hand_without_old_commodity =
    List.filter player.hand ~f:(fun player_commodity ->
      not (Commodity.equal player_commodity old_commodity))
  in
  player.hand <- hand_without_old_commodity @ list_of_new_commodity
;;

let handle_trade t (player : Player.t) commodity_to_trade num_cards =
  let player_hand = player.hand in
  let num_of_commodity =
    List.length
      (List.filter player_hand ~f:(fun commodity ->
         Commodity.equal commodity commodity_to_trade))
  in
  if not (num_of_commodity = num_cards)
  then Core.print_endline "Trade Rejected: Invalid number of cards."
  else if List.mem
            (Hashtbl.keys t.open_trades)
            num_of_commodity
            ~equal:Int.equal
  then (
    let other_player_id, other_commodity =
      Hashtbl.find_exn t.open_trades num_of_commodity
    in
    let other_player = get_player t other_player_id in
    match Player.equal player other_player with
    | true -> Core.print_endline "Trade Rejected: Offer already in the book."
    | false ->
      change_hand
        ~player
        ~old_commodity:commodity_to_trade
        ~new_commodity:other_commodity
        ~num_cards;
      change_hand
        ~player:(get_player t other_player_id)
        ~old_commodity:other_commodity
        ~new_commodity:commodity_to_trade
        ~num_cards;
      Core.printf
        "Trade of %d cards successful between player %d and %d \n"
        num_cards
        player.player_id
        other_player_id)
  else (
    Hashtbl.add_exn
      t.open_trades
      ~key:num_cards
      ~data:(player.player_id, commodity_to_trade);
    Core.print_endline "No matching trade found - offer placed on book")
;;

let win_check (player : Player.t) =
  let first_commodity = List.nth_exn player.hand 0 in
  List.for_all player.hand ~f:(fun commodity ->
    Commodity.equal first_commodity commodity)
;;

let _print_hands t =
  List.iter t.players ~f:(fun player -> Player.print_hand player)
;;

let create_game num_players =
  (* Number of players is equal to the number of commodites traded *)
  let players =
    List.init num_players ~f:(fun player_id ->
      Player.{ player_id = player_id + 1; hand = [] })
  in
  (* Get all the types of commodities that we are trading and initialize with
     quantity 9 for each commodity *)
  let commodities_traded = Hashtbl.create (module Commodity) in
  let types_of_commodities_traded = Commodity.game_commodities num_players in
  List.iter types_of_commodities_traded ~f:(fun commodity ->
    Hashtbl.set commodities_traded ~key:commodity ~data:9);
  let open_trades = Hashtbl.create (module Int) in
  { players
  ; game_state = ref Game_State.In_progress
  ; commodities_traded
  ; open_trades
  }
;;

let game_over t (player : Player.t) =
  t.game_state := Game_over { winner = Some player };
  Core.printf "GAME OVER! Winner: %d \n" player.player_id
;;

let start_game num_players =
  let game = create_game num_players in
  generate_player_hands game;
  let game_continues = ref true in
  while !game_continues do
    Core.print_endline
      "Would you like to trade or end the game? (trade / end) ";
    let response = In_channel.input_line_exn In_channel.stdin in
    if String.equal response "end"
    then (
      game.game_state := Game_State.Game_over { winner = None };
      game_continues := false)
    else (
      Core.print_endline "What is your player ID? ";
      let id = Int.of_string (In_channel.input_line_exn In_channel.stdin) in
      let player = get_player game id in
      Player.print_hand player;
      Core.print_endline "\nWhat commodity would you like to trade? ";
      let commodity_to_trade =
        Commodity.of_string (In_channel.input_line_exn In_channel.stdin)
      in
      Core.print_endline "How many would you like to trade? ";
      let num_cards =
        Int.of_string (In_channel.input_line_exn In_channel.stdin)
      in
      handle_trade game player commodity_to_trade num_cards;
      List.iter game.players ~f:(fun player ->
        match win_check player with
        | false -> ()
        | true ->
          game_over game player;
          game_continues := false);
      print_endline "")
  done
;;

let start =
  Command.basic
    ~summary:"Start a game"
    [%map_open.Command
      let num_players =
        flag "players" (required Command.Param.int) ~doc:"Number of players"
      in
      fun () -> start_game num_players]
;;

let command = Command.group ~summary:"Driver" [ "start", start ]
