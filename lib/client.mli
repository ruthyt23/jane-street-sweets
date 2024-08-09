open! Core
open! Async
open! Curses

val print_hand : Commodity.t list -> unit
val game_over : (Player.t * Commodity.t) list -> string

val handle_trade
  :  conn:(unit -> Rpc.Connection.t Deferred.t)
  -> commodity:Commodity.t
  -> num_cards:chtype
  -> ui:Ui.t
  -> unit Deferred.t

val pull_player_data
  :  ui:Ui.t
  -> conn:(unit -> Rpc.Connection.t Deferred.t)
  -> game_id:chtype
  -> player_id:chtype
  -> (Rpc.Pipe_rpc.Id.t, unit) result Or_error.t Deferred.t

val waiting_room
  :  conn:(unit -> Rpc.Connection.t Deferred.t)
  -> unit Deferred.t

val check_valid_player_count : chtype -> unit

val initial_dispatch_to_server
  :  name:string
  -> host:string
  -> port:chtype
  -> num_players:chtype
  -> (Rpcs.Waiting_room.Response.t * (unit -> Rpc.Connection.t Deferred.t))
       Deferred.t

val command : Async.Command.t
