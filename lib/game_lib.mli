open! Async
open! Core
open! Fzf
module Client = Client
module Server = Server

val get_player : Game.t -> int -> Player.t
val generate_player_hands : Game.t -> unit

val change_hand
  :  player:Player.t
  -> old_commodity:Commodity.t
  -> new_commodity:Commodity.t
  -> num_cards:int
  -> unit

val handle_trade : Game.t -> Player.t -> Commodity.t -> int -> unit
val win_check : Player.t -> bool
val _print_hands : Game.t -> unit
