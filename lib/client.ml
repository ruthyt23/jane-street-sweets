open! Async
open! Core
open! Fzf

module Start_game = struct
  module Query = struct
    type t =
      { num_players : int
      ; name : string
      ; host_and_port : int
      }
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      | Game_started
      | Game_not_started
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"start-game"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
end
