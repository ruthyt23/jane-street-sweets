open! Core
open! Async
open! Fzf

let (connection : Rpc.Connection.t option ref) = ref None
let game_id = ref 0
let player_id = ref 0
let (hand : Commodity.t list ref) = ref []
let (book : (Commodity.t * int) list ref) = ref []

let print_hand hand =
  print_endline "Current hand: ";
  List.iter hand ~f:(fun commodity ->
    printf "%s  " (Commodity.to_string commodity))
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

let make_trades ~conn =
  let num_cards_string_list =
    List.init 4 ~f:(fun num -> Int.to_string (num + 1))
  in
  Core.printf "\nWhat commodity would you like to trade? ";
  let commodity = Stdlib.read_line () |> Commodity.of_string in
  Core.print_endline "How many would you like to trade? (1-4) ";
  let selection_for_numcards = Pick_from.inputs num_cards_string_list in
  let%bind num_cards_opt =
    pick_one selection_for_numcards ~prompt_at_top:() ~height:7
  in
  match num_cards_opt |> ok_exn with
  | None -> return ()
  | Some cards ->
    printf "Number of cards: %s\n" cards;
    let num_cards = Int.of_string cards in
    handle_trade ~conn ~commodity ~num_cards
;;

let game_over winner_list =
  if List.length winner_list = 1
  then Core.print_endline "GAME OVER! WINNER: "
  else Core.print_endline "GAME OVER! WINNERS: ";
  List.iter winner_list ~f:(fun ((player : Player.t), commodity) ->
    Core.print_endline
      [%string "%{player.player_name#String} with %{commodity#Commodity}"])
;;

let pull_game_state ~conn =
  Deferred.repeat_until_finished () (fun _ ->
    let (query : Rpcs.Game_state.Query.t) = !game_id in
    let%bind conn_bind = conn () in
    let%bind (response : Rpcs.Game_state.Response.t) =
      Rpc.Rpc.dispatch_exn Rpcs.Game_state.rpc conn_bind query
    in
    match response with
    | Rpcs.Game_state.Response.In_progress ->
      if List.is_empty !hand
      then Core.print_endline "\n*** WELCOME TO PIT! ***";
      let%bind () = make_trades ~conn in
      return (`Repeat ())
    | Rpcs.Game_state.Response.Game_over winner_list ->
      game_over winner_list;
      return (`Finished ())
    | Rpcs.Game_state.Response.Waiting ->
      let span = Time_float.Span.of_ms 250.0 in
      let%bind () = Async.after span in
      return (`Repeat ()))
;;

let pull_player_data ~conn ~game_id ~player_id =
  let (query : Rpcs.Player_game_data.Query.t) =
    { Rpcs.Player_game_data.Query.game_id
    ; Rpcs.Player_game_data.Query.player_id
    }
  in
  Rpc.Pipe_rpc.dispatch_iter
    Rpcs.Player_game_data.rpc
    conn
    query
    ~f:(fun response ->
      match response with
      | Closed _ ->
        print_endline "pipe closed.";
        Rpc.Pipe_rpc.Pipe_response.Wait (return ())
      | Update message ->
        let ( (current_book : (Commodity.t * int) list)
            , player_hand
            , winner_list )
          =
          message.current_book, message.player_hand, message.winner_list
        in
        (* print_s [%sexp (!hand : Commodity.t list)]; *)
        (match winner_list with
         | Some winner_list ->
           game_over winner_list;
           Rpc.Pipe_rpc.Pipe_response.Wait (return ())
         | None ->
           if (not (List.length current_book = List.length !book))
              || List.exists2_exn
                   current_book
                   !book
                   ~f:(fun (commodity1, num1) (commodity2, num2) ->
                     not
                       (Commodity.equal commodity1 commodity2 || num1 = num2))
           then (
             let _ = Sys.command "clear" in
             book := current_book;
             print_endline "*** UPDATED BOOK ***";
             print_s [%sexp (!book : (Commodity.t * int) list)]);
           (* if not (List.equal Commodity.equal player_hand !hand) then ( *)
           hand := player_hand;
           print_endline "*** UPDATED HAND ***";
           print_s [%sexp (!hand : Commodity.t list)];
           Rpc.Pipe_rpc.Pipe_response.Continue))
;;

let book_data =
  Command.async
    ~summary:"Live updating of book data"
    (let%map_open.Command () = return ()
     and host = flag "-host" (required string) ~doc:"_ host name"
     and int_port = flag "-port" (required int) ~doc:"_ port to listen on"
     and game = flag "-game" (required int) ~doc:"_ name of player"
     and player = flag "-player" (required int) ~doc:"_ name of player" in
     fun () ->
       let host_and_port = Host_and_port.create ~host ~port:int_port in
       let%bind conn_bind =
         Rpc.Connection.client
           (Tcp.Where_to_connect.of_host_and_port host_and_port)
       in
       let conn = Result.ok_exn conn_bind in
       let%bind _ = pull_player_data ~conn ~game_id:game ~player_id:player in
       Deferred.never ())
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
       if num_players < 3 || num_players > 9
       then failwith "Invalid number of players: must be between 3-9."
       else (
         let host_and_port = Host_and_port.create ~host ~port:int_port in
         let join_game_query =
           { Rpcs.Waiting_room.Query.name
           ; Rpcs.Waiting_room.Query.num_players
           }
         in
         let conn () =
           Rpc.Connection.client
             (Tcp.Where_to_connect.of_host_and_port host_and_port)
           >>| Result.ok_exn
         in
         let%bind conn_bind = conn () in
         let%bind (initial_game_data : Rpcs.Waiting_room.Response.t) =
           Rpc.Rpc.dispatch_exn
             Rpcs.Waiting_room.rpc
             conn_bind
             join_game_query
         in
         game_id := initial_game_data.game_id;
         player_id := initial_game_data.player_id;
         printf
           "Connected to host %s!\nGame ID: %d\nPlayer ID: %d"
           host
           !game_id
           !player_id;
         pull_game_state ~conn))
;;

let command =
  Command.group
    ~summary:"Pit Player"
    [ "join-game", connect_to_server; "book-data", book_data ]
;;
