open! Core
open! Async
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
      | Game_starts of t
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
      | Trade_successful of Commodity.t list
      | In_book
      | Trade_rejected of string
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t =
      { current_hand : Commodity.t list
      ; commodity : Commodity.t
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
    type t = (Player.t * Commodity.t) list [@@deriving sexp_of, bin_io]
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

let waiting_handle (_client : unit) (query : Waiting_room.Query.t) =
  let game, piece = query.game, query.you_play in
  let response =
    { Rpcs.Take_turn.Response.piece
    ; Rpcs.Take_turn.Response.position = Exercises.minimax game ~me:piece
    }
  in
  return response
;;

let in_progress_handle (_client : unit) (query : In_progress.Query.t) =
  match query with
  | In_book ->
    Core.print_endline "No matching offers found - offer placed on book."
  | Trade_rejected message -> Core.print_endline message
  | Trade_successful new_hand -> ignore new_hand
;;

let game_over_handle (_client : unit) (query : Game_over.Query.t) =
  Core.print_endline "GAME OVER!";
  if List.length query = 1
  then Core.print_endline "WINNER: "
  else Core.print_endline "WINNERS: ";
  List.iter query ~f:(fun (player, commodity) ->
    Core.print_endline
      [%string "%{player.player_id#Int} with %{commodity#Commodity}"])
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ Rpc.Rpc.implement Waiting_room.rpc waiting_handle
      ; Rpc.Rpc.implement In_progress.rpc in_progress_handle
      ; Rpc.Rpc.implement Game_over.rpc game_over_handle
      ]
;;

let join_game =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             (* This constructs the "client" values which are passed to the
                implementation function above. We're just using unit for
                now. *)
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;

let command = Command.group ~summary:"Pit Player" [ "join-game", join_game ]
