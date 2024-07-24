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
