open! Core
open! Async
open! Fzf
open! Curses

let (connection : Rpc.Connection.t option ref) = ref None
let game_id = ref 0
let player_id = ref 0
let (hand : Commodity.t list ref) = ref []
let (book : (int * Commodity.t * int) list ref) = ref []

let print_hand hand =
  print_endline "Current hand: ";
  List.iter hand ~f:(fun commodity ->
    printf "%s  " (Commodity.to_string commodity))
;;

let game_over (winner_list : (Player.t * Commodity.t) list) =
  let game_over_message =
    match List.length winner_list = 1 with
    | true -> "GAME OVER! WINNER: "
    | false -> "GAME OVER! WINNERS: "
  in
  List.fold
    winner_list
    ~init:game_over_message
    ~f:(fun message player_commodity_pair ->
      let winner, _ = player_commodity_pair in
      message ^ " " ^ winner.player_name)
;;

let handle_trade ~conn ~commodity ~num_cards =
  let query =
    { Rpcs.Make_trade.Query.player_id = !player_id
    ; Rpcs.Make_trade.Query.game_id = !game_id
    ; Rpcs.Make_trade.Query.commodity
    ; Rpcs.Make_trade.Query.quantity = num_cards
    }
  in
  let%bind conn_bind = conn () in
  let%bind (response : Rpcs.Make_trade.Response.t) =
    Rpc.Rpc.dispatch_exn Rpcs.Make_trade.rpc conn_bind query
  in
  (match response with
   | Trade_successful _ -> ()
   | In_book ->
     Core.print_endline "No matching trade found - offer placed on book"
   | Trade_rejected message -> Core.print_endline message);
  return ()
;;

(* Helper for get_book_and_hand data *)
let pull_player_data ~(ui : Ui.t) ~conn ~game_id ~player_id =
  let (query : Rpcs.Player_game_data.Query.t) =
    { Rpcs.Player_game_data.Query.game_id
    ; Rpcs.Player_game_data.Query.player_id
    }
  in
  let%bind conn_bind = conn () in
  Rpc.Pipe_rpc.dispatch_iter
    Rpcs.Player_game_data.rpc
    conn_bind
    query
    ~f:(fun response ->
      match response with
      | Closed _ -> Rpc.Pipe_rpc.Pipe_response.Wait (return ())
      | Update update ->
        (match update with
         | Game_won winner_list ->
           let message = game_over winner_list in
           Ui.update_game_over ui message;
           Rpc.Pipe_rpc.Pipe_response.Wait (return ())
         | Book_updated current_book ->
           book := current_book;
           Ui.update_book ui current_book player_id;
           Rpc.Pipe_rpc.Pipe_response.Continue
         | Hand_updated current_hand ->
           hand := current_hand;
           Ui.update_hand ui current_hand;
           Rpc.Pipe_rpc.Pipe_response.Continue))
;;

let waiting_room ~conn =
  Deferred.repeat_until_finished () (fun _ ->
    let (query : Rpcs.Game_state.Query.t) = !game_id in
    let%bind conn_bind = conn () in
    let%bind (response : Rpcs.Game_state.Response.t) =
      Rpc.Rpc.dispatch_exn Rpcs.Game_state.rpc conn_bind query
    in
    match response with
    | Rpcs.Game_state.Response.Waiting ->
      let span = Time_float.Span.of_ms 250.0 in
      let%bind () = Async.after span in
      return (`Repeat ())
    | _ -> return (`Finished ()))
;;

(* Helper function for connect to server *)
let check_valid_player_count num_players =
  match num_players < 3 || num_players > 9 with
  | true -> failwith "Invalid number of players: must be between 3-9"
  | false -> ()
;;

(* Helper function for connect to server *)
let initial_dispatch_to_server ~name ~host ~port ~num_players =
  let host_and_port = Host_and_port.create ~host ~port in
  let join_game_query =
    { Rpcs.Waiting_room.Query.name; Rpcs.Waiting_room.Query.num_players }
  in
  let conn () =
    Rpc.Connection.client
      (Tcp.Where_to_connect.of_host_and_port host_and_port)
    >>| Result.ok_exn
  in
  let%bind conn_bind = conn () in
  let%bind (initial_game_data : Rpcs.Waiting_room.Response.t) =
    Rpc.Rpc.dispatch_exn Rpcs.Waiting_room.rpc conn_bind join_game_query
  in
  return (initial_game_data, conn)
;;

let connect_to_server =
  Command.async
    ~summary:"Join game"
    (let%map_open.Command () = return ()
     and name = flag "-name" (required string) ~doc:"_ name of player"
     and num_players =
       flag
         "-players"
         (required int)
         ~doc:"_ game size the user would like to play in"
     and host = flag "-host" (required string) ~doc:"_ host name"
     and int_port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       check_valid_player_count num_players;
       let%bind response_from_server =
         initial_dispatch_to_server ~name ~host ~port:int_port ~num_players
       in
       let initial_game_data, conn = response_from_server in
       game_id := initial_game_data.game_id;
       player_id := initial_game_data.player_id;
       printf
         "Connected to host %s!\nGame ID: %d\nPlayer ID: %d"
         host
         !game_id
         !player_id;
       let%bind _ = waiting_room ~conn in
       let ui = Ui.init () in
       Ui.update_hand ui !hand;
       Ui.update_book ui !book !player_id;
       let%bind _ =
         (* pass in the UI when the time comes *)
         pull_player_data ~ui ~conn ~game_id:!game_id ~player_id:!player_id
       in
       Deferred.repeat_until_finished () (fun () ->
         let%bind commodity, num_cards = Ui.manage_user_input ui in
         let%map _ = handle_trade ~conn ~commodity ~num_cards in
         `Repeat ()))
;;

let command =
  Command.group ~summary:"Pit Player" [ "join-game", connect_to_server ]
;;
