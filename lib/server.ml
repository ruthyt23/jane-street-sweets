open! Core
open! Async
open! Fzf

module Player_id_manager = struct
  type t = int ref

  let create () = ref 1

  let next_id t =
    let this_id = !t in
    incr t;
    this_id
  ;;
end

module Game_id_manager = struct
  type t = int ref

  let create () = ref 100

  let next_id t =
    let this_id = !t in
    incr t;
    this_id
  ;;
end

(* Module Update will give players updates for the following three things 1)
   Their player hand if it changes 2) The book log if it changes 3) Game
   winners if there is a winner *)
module Update = struct
  type t = Rpcs.Player_game_data.Response.t
end

module Game_manager = struct
  type t =
    { games_waiting_to_start : (int, Game.t) Hashtbl.t
    ; games_that_have_started : Game.t Queue.t
    }

  let create () =
    let games_waiting_to_start = Hashtbl.create (module Int) in
    let _ =
      List.init 6 ~f:(fun num_players ->
        Hashtbl.set
          games_waiting_to_start
          ~key:(3 + num_players)
          ~data:(Game.create_empty_game num_players))
    in
    { games_waiting_to_start; games_that_have_started = Queue.create () }
  ;;
end

let game_id_manager = Game_id_manager.create ()
let player_id_manager = Player_id_manager.create ()
let game_manager = Game_manager.create ()

let waiting_handle (_client : unit) (query : Rpcs.Waiting_room.Query.t) =
  let response_player_id = Player_id_manager.next_id player_id_manager in
  let player_obj = Player.create_player response_player_id query.name in
  let game_to_join =
    Hashtbl.find_exn game_manager.games_waiting_to_start query.num_players
  in
  let response_game_id = game_to_join.game_id in
  Game.add_player_to_game game_to_join player_obj;
  if equal (List.length game_to_join.players) query.num_players
  then (
    Game.start_game game_to_join;
    printf "Game %d has started\n" response_game_id;
    Queue.enqueue game_manager.games_that_have_started game_to_join;
    Hashtbl.set
      game_manager.games_waiting_to_start
      ~key:query.num_players
      ~data:
        (Game.create_empty_game (Game_id_manager.next_id game_id_manager)));
  printf
    "%s has joined game %d and their player_id is %d\n"
    query.name
    response_game_id
    response_player_id;
  Deferred.return
    { Rpcs.Waiting_room.Response.game_id = response_game_id
    ; player_id = response_player_id
    }
;;

let find_game (game_id : int) =
  Queue.find game_manager.games_that_have_started ~f:(fun game ->
    equal game.game_id game_id)
;;

let game_data_handle (_client : unit) (query : Rpcs.Game_state.Query.t) =
  let game_opt = find_game query in
  match game_opt with
  | None -> Deferred.return Rpcs.Game_state.Response.Waiting
  | Some game ->
    (match Game.has_winners game with
     | true ->
       return
         (Rpcs.Game_state.Response.Game_over
            (Game.get_list_of_winning_players game))
     | false -> return Rpcs.Game_state.Response.In_progress)
;;

let get_player_hand_update (game : Game.t) player_id =
  let player_hand = Game.get_hand_for_player game player_id in
  Rpcs.Player_game_data.Response.Hand_updated player_hand
;;

let get_book_update (game : Game.t) =
  let list_of_trade_amounts = Hashtbl.keys game.open_trades in
  let response_book =
    List.map list_of_trade_amounts ~f:(fun amount_to_trade ->
      let _, commodity = Hashtbl.find_exn game.open_trades amount_to_trade in
      commodity, amount_to_trade)
  in
  Rpcs.Player_game_data.Response.Book_updated response_book
;;

let get_winners_update (game : Game.t) =
  let list_of_winners = Game.get_list_of_winning_players game in
  Rpcs.Player_game_data.Response.Game_won list_of_winners
;;

let player_game_data_handle
  (_client : unit)
  (query : Rpcs.Player_game_data.Query.t)
  =
  let game_opt = find_game query.game_id in
  match game_opt with
  | None -> return (Error ())
  | Some game ->
    print_endline "Pipe created";
    return
      (Ok
         (Async_kernel.Pipe.create_reader
            ~close_on_exception:true
            (fun writer ->
               let starting_hand =
                 get_player_hand_update game query.player_id
               in
               let%bind () = Pipe.write writer starting_hand in
               Game.add_to_game_listeners game query.player_id (fun update ->
                 Pipe.write writer update);
               Deferred.never ())))
;;

let ping_book_updates (game : Game.t) =
  let updated_book = get_book_update game in
  let%bind () =
    Deferred.List.iter
      ~how:`Parallel
      game.game_listeners
      ~f:(fun player_listener_pair ->
        let _, listener = player_listener_pair in
        listener updated_book)
  in
  return ()
;;

let ping_player_hand_update (game : Game.t) (player_id_to_ping : int) =
  let updated_hand = get_player_hand_update game player_id_to_ping in
  let listener_pair_to_ping =
    List.find game.game_listeners ~f:(fun player_listener_pair ->
      let player, _ = player_listener_pair in
      Int.equal player player_id_to_ping)
  in
  match listener_pair_to_ping with
  | None -> failwith "Trying to ping to a listener that doesn't exist"
  | Some listener_pair ->
    let _, listener = listener_pair in
    listener updated_hand
;;

let ping_game_won_updates (game : Game.t) =
  let updated_winners = get_winners_update game in
  let%bind () =
    Deferred.List.iter
      ~how:`Parallel
      game.game_listeners
      ~f:(fun player_listener_pair ->
        let _, listener = player_listener_pair in
        listener updated_winners)
  in
  return ()
;;

let make_trade_handle (_client : unit) (query : Rpcs.Make_trade.Query.t) =
  printf
    "Player %d is trying to trade %d of commodity %s\n"
    query.player_id
    query.quantity
    (Commodity.to_string query.commodity);
  let game_opt = find_game query.game_id in
  match game_opt with
  | None -> failwith "trying to trade with no game"
  | Some game ->
    let players_list = game.players in
    let player_obj =
      List.find_exn players_list ~f:(fun player ->
        equal player.player_id query.player_id)
    in
    let%bind result =
      Game.handle_trade game player_obj query.commodity query.quantity
    in
    (match result with
     | Rpcs.Make_trade.Response.In_book ->
       print_endline "Order placed in book";
       let%bind () = ping_book_updates game in
       Deferred.return result
     | Rpcs.Make_trade.Response.Trade_rejected msg ->
       print_endline msg;
       Deferred.return result
     | Rpcs.Make_trade.Response.Trade_successful players_involved ->
       print_endline "Order Successful";
       let player_1, player_2 = players_involved in
       let%bind () = ping_player_hand_update game player_1 in
       let%bind () = ping_player_hand_update game player_2 in
       let%bind () = ping_book_updates game in
       (match Game.has_winners game with
        | false -> Deferred.return result
        | true ->
          let%bind () = ping_game_won_updates game in
          Deferred.return result))
;;

Core.Result.ok

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ Rpc.Rpc.implement Rpcs.Waiting_room.rpc waiting_handle
      ; Rpc.Rpc.implement Rpcs.Game_state.rpc game_data_handle
      ; Rpc.Pipe_rpc.implement
          Rpcs.Player_game_data.rpc
          player_game_data_handle
      ; Rpc.Rpc.implement Rpcs.Make_trade.rpc make_trade_handle
      ]
;;

let start_game =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     and port = flag "-port" (required int) ~doc:"_ port to listen\n   on" in
     print_endline "About to start server...";
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

let command =
  Command.group ~summary:"Pit Server" [ "start-server", start_game ]
;;
