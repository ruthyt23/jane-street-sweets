open! Core
open! Async

module Player_id_manager : sig
  type t = int ref

  val create : unit -> int ref
  val next_id : t -> int
end

module Game_id_manager : sig
  type t = int ref

  val create : unit -> int ref
end

module Update : sig
  type t = Rpcs.Player_game_data.Response.t
end

module Game_manager : sig
  type t =
    { games_waiting_to_start : (int, Game.t) Hashtbl.t
    ; games_that_have_started : Game.t Queue.t
    }

  val create : unit -> t
end

val find_game : int -> Game.t option

val waiting_handle
  :  unit
  -> Rpcs.Waiting_room.Query.t
  -> Rpcs.Waiting_room.Response.t Deferred.t

val game_data_handle
  :  unit
  -> Rpcs.Game_state.Query.t
  -> Rpcs.Game_state.Response.t Deferred.t

val player_game_data_handle
  :  unit
  -> Rpcs.Player_game_data.Query.t
  -> (Update.t Pipe.Reader.t, unit) result Deferred.t

val make_trade_handle
  :  unit
  -> Rpcs.Make_trade.Query.t
  -> Rpcs.Make_trade.Response.t Deferred.t

val command : Async.Command.t
