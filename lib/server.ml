open! Async
open! Core
open! Fzf

module Waiting_room = struct
  type t =
    { target_num_players : int
    ; current_games : (int, Game_lib.t) Hashtbl.t
    }

  let check_for_valid_game t =
    let curr_num_players = Queue.length t.current_players in
    equal t.target_num_players curr_num_players
  ;;

  let add_new_player t new_player =
    Queue.enqueue t.current_players new_player
  ;;

  (* Add functionality to remove a certain player if they disconnect later
     on*)
end

let start_game_impl client (query : Client.Start_game.Query.t) =
  (* Make this connection confirmation message more descriptive later on *)
  printf "A client has joined the game";
  ignore client;
  ignore query
;;

let command =
  Command.async
    ~summary:"Client"
    (let%map_open.Command () = return ()
     and port = flag "-port" (required int) ~doc:"INT server port" in
     fun () ->
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;
