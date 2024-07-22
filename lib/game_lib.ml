open! Async
open! Core
open! Fzf

module Commodity = struct
  type t =
    | Fairlife
    | Pocky
    | Water
    | Boba
    | Mango
    | Starburst
    | Hint
    | Gum
    | Kitkat
  [@@deriving equal, enumerate, compare, sexp_of, hash]

  (* game_commodities --> generates the n commodities that will be traded in
     the game based on n players *)
  let game_commodities num_players =
    let commodity_array = Array.of_list all in
    Array.permute commodity_array;
    List.init num_players ~f:(fun index -> Array.get commodity_array index)
  ;;

  let to_string t =
    match t with
    | Fairlife -> "Fairlife"
    | Pocky -> "Pocky"
    | Water -> "Water"
    | Boba -> "Boba"
    | Mango -> "Mango"
    | Starburst -> "Starburst"
    | Hint -> "Hint"
    | Gum -> "Gum"
    | Kitkat -> "Kitkat"
  ;;

  let of_string str =
    match str with
    | "Fairlife" -> Fairlife
    | "Pocky" -> Pocky
    | "Water" -> Water
    | "Boba" -> Boba
    | "Mango" -> Mango
    | "Starburst" -> Starburst
    | "Hint" -> Hint
    | "Gum" -> Gum
    | "Kitkat" -> Kitkat
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
    printf "Hand for player %d: " t.player_id;
    List.iter t.hand ~f:(fun commodity ->
      printf "%s  " (Commodity.to_string commodity))
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
  | None -> failwith "No player matches given ID"
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
  if num_of_commodity < num_cards || num_cards < 1 || num_cards > 4
  then print_endline "Trade Rejected: Invalid number of cards - must be 1-4."
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
    | true -> print_endline "Trade Rejected: Offer already in the book."
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
      printf
        "Trade of %d cards successful between player %d and %d \n"
        num_cards
        player.player_id
        other_player_id)
  else (
    Hashtbl.add_exn
      t.open_trades
      ~key:num_cards
      ~data:(player.player_id, commodity_to_trade);
    print_endline "No matching trade found - offer placed on book")
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
  printf "GAME OVER! Winner: %d \n" player.player_id
;;

let start_game num_players =
  let game = create_game num_players in
  generate_player_hands game;
  let player_string_list =
    List.map game.players ~f:(fun player -> Int.to_string player.player_id)
  in
  let num_cards_string_list =
    List.init 4 ~f:(fun num -> Int.to_string (num + 1))
  in
  let game_continues = ref true in
  print_endline "*** WELCOME TO PIT! ***\nGame Start\n";
  Deferred.repeat_until_finished () (fun _ ->
    print_endline "What is your player ID? ";
    let selection_for_playerid = Pick_from.inputs player_string_list in
    let%bind string_id_opt =
      pick_one
        selection_for_playerid
        ~prompt_at_top:()
        ~height:(num_players + 3)
    in
    match string_id_opt |> ok_exn with
    | None -> return (`Finished ())
    | Some string_id ->
      let id = Int.of_string string_id in
      let player = get_player game id in
      Player.print_hand player;
      print_endline "\nWhat commodity would you like to trade? ";
      let commodities_string_list =
        List.dedup_and_sort player.hand ~compare:Commodity.compare
        |> List.map ~f:(fun commodity -> Commodity.to_string commodity)
      in
      let selection_for_commodities =
        Pick_from.inputs commodities_string_list
      in
      let%bind commodity_opt =
        pick_one
          selection_for_commodities
          ~prompt_at_top:()
          ~height:(List.length commodities_string_list + 3)
      in
      (match commodity_opt |> ok_exn with
       | None -> return (`Finished ())
       | Some commodity ->
         print_endline "How many would you like to trade? (1-4)";
         let selection_for_numcards =
           Pick_from.inputs num_cards_string_list
         in
         let%map num_cards_opt =
           pick_one selection_for_numcards ~prompt_at_top:() ~height:7
         in
         (match num_cards_opt |> ok_exn with
          | None -> `Finished ()
          | Some cards ->
            let num_cards = Int.of_string cards in
            handle_trade
              game
              player
              (Commodity.of_string commodity)
              num_cards;
            List.iter game.players ~f:(fun player ->
              match win_check player with
              | false -> ()
              | true ->
                game_over game player;
                game_continues := false);
            print_endline "";
            (match !game_continues with
             | true -> `Repeat ()
             | false -> `Finished ()))))
;;

let start =
  Async.Command.async
    ~summary:"Start a game"
    [%map_open.Command
      let num_players =
        flag "players" (required Command.Param.int) ~doc:"Number of players"
      in
      fun () ->
        if num_players < 3 || num_players > 9
        then failwith "Invalid number of players: must be 3-9"
        else start_game num_players]
;;

let command = Command.group ~summary:"Driver" [ "start", start ]
