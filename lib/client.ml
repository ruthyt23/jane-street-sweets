open! Async
open! Core
open! Fzf

module Waiting_room = struct
  module Query = struct
    type t =
      { num_players : int
      ; name : string
      ; host_and_port : Host_and_port.t
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      | Waiting
      | Game_starts
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"waiting-room"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module In_progress = struct
  module Query = struct
    type t =
      | Trade_successful of int
      | In_book of int
      | Trade_rejected of int
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      { commodity : Commodity.t
      ; num_cards : int
      }
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"in-progress"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Game_over = struct
  module Query = struct
    type t = { winners : (Player.t * Commodity.t) list }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = unit [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"game-over"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end
