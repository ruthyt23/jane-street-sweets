open! Core
open! Async
open! Fzf

let join_game =
  Command.async
    ~summary:"Join game"
    (let%map_open.Command () = return ()
     and name = flag "-name" (required string) ~doc:"_ name of player"
     and int_port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       let port = Host_and_port.create ~host:name ~port:int_port in
       let join_game_query = name in
       let%bind join_game =
         Rpc.Connection.with_client
           (Tcp.Where_to_connect.of_host_and_port port)
           (fun conn ->
              Rpc.Rpc.dispatch_exn Rpcs.Waiting_room.rpc conn join_game_query)
       in
       print_s
         [%message
           "join_game"
             (join_game : (Rpcs.Waiting_room.Response.t, exn) Result.t)])
;;

let command = Command.group ~summary:"Pit Player" [ "join-game", join_game ]
