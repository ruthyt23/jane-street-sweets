open! Core
open! Async
open! Fzf

module Waiting_room = struct
  module Query = struct
    type t = string [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = int [@@deriving sexp_of, bin_io]
  end
  (* future extension: add option to pick number of players + multiple
     games *)

  let rpc =
    Rpc.Rpc.create
      ~name:"waiting-room"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Game_data = struct
  module Query = struct
    type t = int [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      | Waiting
      | In_progress of Commodity.t list
      | Game_over of (Player.t * Commodity.t) list
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"game-data"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Make_trade = struct
  module Query = struct
    type t =
      { player_id : int
      ; commodity : Commodity.t
      ; quantity : int
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      | Trade_successful
      | In_book
      | Trade_rejected of string
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"make-trade"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end
