open! Core
open! Async
open! Fzf

let waiting_handle (_client : unit) (query : Waiting_room.Query.t) =
  
;; 

let game_data_handle (_client : unit) (query : Game_data.Query.t) =
;;

let make_trade_handle (_client : unit) (query : Make_trade.Query.t) =
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ Rpc.Rpc.implement Waiting_room.rpc waiting_handle
      ; Rpc.Rpc.implement Game_data.rpc game_data_handle
      ; Rpc.Rpc.implement Make_trade.rpc make_trade_handle
      ]
;;

let start_game =
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
