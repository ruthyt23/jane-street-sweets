open! Async
open! Core
open! Fzf
module Client = Client
module Server = Server

module Game_State : sig
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

val get_player : t -> int -> Player.t
val generate_player_hands : t -> unit

val change_hand
  :  player:Player.t
  -> old_commodity:Commodity.t
  -> new_commodity:Commodity.t
  -> num_cards:int
  -> unit

val handle_trade : t -> Player.t -> Commodity.t -> int -> unit
val win_check : Player.t -> bool
val _print_hands : t -> unit
val create_game : int -> t
val game_over : t -> Player.t -> unit
val start_game : int -> unit Deferred.t
