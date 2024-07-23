open! Async
open! Core
open! Fzf

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
