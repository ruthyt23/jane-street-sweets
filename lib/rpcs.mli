open! Core
open! Async

module Waiting_room : sig
  module Query : sig
    type t =
      { name : string
      ; num_players : int
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t =
      { game_id : int
      ; player_id : int
      }
    [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module Game_state : sig
  module Query : sig
    type t = int [@@deriving sexp_of, bin_io] (* game id *)
  end

  module Response : sig
    type t =
      | Waiting
      | In_progress
      | Game_over of (Player.t * Commodity.t) list
    [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module Player_game_data : sig
  (* game id, player id*)
  module Query : sig
    type t =
      { game_id : int
      ; player_id : int
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t =
      (* Traded n of Commodity.t for other Commodity.t *)
      | Trade_went_through of (int * Commodity.t * Commodity.t)
      | Book_updated of (int * Commodity.t * int) list
      | Hand_updated of Commodity.t list
      | Game_won of (Player.t * Commodity.t) list
    [@@deriving sexp_of, bin_io]
  end

  module Error : sig
    type t = unit [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t, unit) Rpc.Pipe_rpc.t
end

module Make_trade : sig
  module Query : sig
    type t =
      { player_id : int
      ; game_id : int
      ; commodity : Commodity.t
      ; quantity : int
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t =
      (* Trade happened between player1 and player2 *)
      | Trade_successful of
          ((Player.Player_id.t * (Commodity.t * int))
          * (Player.Player_id.t * (Commodity.t * int)))
      | In_book
      | Trade_rejected of string
    [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end
