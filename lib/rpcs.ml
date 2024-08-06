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
    Rpc.Rpc.create
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
    Rpc.Rpc.create
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
      | Book_updated of (int * Commodity.t * int) list
      | Hand_updated of Commodity.t list
      | Game_won of (Player.t * Commodity.t) list
    [@@deriving sexp_of, bin_io]
  end

  module Error = struct
    type t = unit [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"player-game-data"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
      ~bin_error:Error.bin_t
      ()
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
      (* Trade happened between player1 and player2 *)
      | Trade_successful of (int * int)
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
