open! Async
open! Core

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

val book_to_string : (int * Commodity.t * int) list -> int -> string
val get_hand_for_player : t -> int -> Commodity.t list
val win_checker : Player.t -> bool
val get_list_of_winning_players : t -> (Player.t * Commodity.t) list
val has_winners : t -> bool
val get_player : t -> int -> Player.t

val change_hand
  :  player:Player.t
  -> old_commodity:Commodity.t
  -> new_commodity:Commodity.t
  -> num_cards:int
  -> unit

val handle_trade
  :  t
  -> Player.t
  -> Commodity.t
  -> int
  -> Rpcs.Make_trade.Response.t Deferred.t

val generate_player_hands : t -> unit
val create_empty_game : int -> t

val add_to_game_listeners
  :  t
  -> int
  -> (Rpcs.Player_game_data.Response.t -> unit Deferred.t)
  -> unit

val start_game : t -> unit
val add_player_to_game : t -> Player.t -> unit
val get_player_hand_update : t -> int -> Rpcs.Player_game_data.Response.t
val get_book_update : t -> Rpcs.Player_game_data.Response.t
val get_winners_update : t -> Rpcs.Player_game_data.Response.t

val get_trade_went_through_update
  :  int
  -> Commodity.t
  -> Commodity.t
  -> Rpcs.Player_game_data.Response.t

val ping_book_updates : t -> unit Deferred.t
val ping_player_hand_update : t -> int -> unit Deferred.t
val ping_game_won_updates : t -> unit Deferred.t

val ping_trade_went_through_update
  :  t
  -> int
  -> int
  -> Commodity.t
  -> Commodity.t
  -> unit Deferred.t
