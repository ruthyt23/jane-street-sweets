open! Core
open! Async
open! Fzf

module Waiting_room = struct
  module Query = struct
    type t =
      { name : string
      ; num_players : int
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      { game_id : int
      ; player_id : int
      }
    [@@deriving sexp_of, bin_io]
  end
  (* future extension: add option to pick number of players + multiple
     games *)

  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"waiting-room"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Game_state = struct
  module Query = struct
    type t = int [@@deriving sexp_of, bin_io] (* game id *)
  end

  module Response = struct
    type t =
      | Waiting
      | In_progress
      | Game_over of (Player.t * Commodity.t) list
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"game-state"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Player_game_data = struct
  (* game id, player id*)
  module Query = struct
    type t =
      { game_id : int
      ; player_id : int
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      { current_book : (Commodity.t * int) list
      ; player_hand : Commodity.t list
      ; winner_list : (Player.t * Commodity.t) list option
      }
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"player-game-data"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end

module Make_trade = struct
  module Query = struct
    type t =
      { player_id : int
      ; game_id : int
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
    Rpc.Pipe_rpc.create
      ~name:"make-trade"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end
