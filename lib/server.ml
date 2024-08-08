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

  (* Start from 7 because games 0-6 are created when the server starts *)
  let create () = ref 7

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

let find_game (game_id : int) =
  Queue.find game_manager.games_that_have_started ~f:(fun game ->
    equal game.game_id game_id)
;;

let waiting_handle (_client : unit) (query : Rpcs.Waiting_room.Query.t) =
  let response_player_id = Player_id_manager.next_id player_id_manager in
  let player_obj = Player.create_player response_player_id query.name in
  let game_to_join =
    Hashtbl.find_exn game_manager.games_waiting_to_start query.num_players
  in
  let response_game_id = game_to_join.game_id in
  Game.add_player_to_game game_to_join player_obj;
  (* Check if game is ready to start *)
  (* if the game is full, then replace the game in the hashtable of games
     waiting to start *)
  if List.length game_to_join.players = query.num_players
  then (
    game_to_join.game_full <- true;
    Game.start_game game_to_join;
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
  if game_to_join.game_full
  then printf "Game %d has started\n" response_game_id;
  Deferred.return
    { Rpcs.Waiting_room.Response.game_id = response_game_id
    ; player_id = response_player_id
    }
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

let player_game_data_handle
  (_client : unit)
  (query : Rpcs.Player_game_data.Query.t)
  =
  let game_opt = find_game query.game_id in
  match game_opt with
  | None -> return (Error ())
  | Some game ->
    return
      (Ok
         (Async_kernel.Pipe.create_reader
            ~close_on_exception:true
            (fun writer ->
               printf
                 "Listener created for player %d in game %d\n"
                 query.player_id
                 query.game_id;
               let starting_hand =
                 Game.get_player_hand_update game query.player_id
               in
               let%bind () = Pipe.write writer starting_hand in
               Game.add_to_game_listeners game query.player_id (fun update ->
                 Pipe.write writer update);
               Deferred.never ())))
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
    let player_obj = Game.get_player game query.player_id in
    let%bind result =
      Game.handle_trade game player_obj query.commodity query.quantity
    in
    (match result with
     | Rpcs.Make_trade.Response.In_book ->
       print_endline "Order placed in book";
       let%bind () = Game.ping_book_updates game in
       Deferred.return result
     | Rpcs.Make_trade.Response.Trade_rejected msg ->
       print_endline msg;
       Deferred.return result
     | Rpcs.Make_trade.Response.Trade_successful trade_data ->
       print_endline "Order Successful";
       let player_1_data, player_2_data = trade_data in
       let player_1, player_1_trade = player_1_data in
       let player_1_commodity, player_1_quantity = player_1_trade in
       let player_2, player_2_trade = player_2_data in
       let player_2_commodity, player_2_quantity = player_2_trade in
       let%bind () = Game.ping_book_updates game in
       let%bind () = Game.ping_player_hand_update game player_1 in
       let%bind () = Game.ping_player_hand_update game player_2 in
       let%bind () =
         Game.ping_trade_went_through_update
           game
           player_1_quantity
           player_1_commodity
       in
       let%bind () =
         Game.ping_trade_went_through_update
           game
           player_2_quantity
           player_2_commodity
       in
       (match Game.has_winners game with
        | false -> Deferred.return result
        | true ->
          let%bind () = Game.ping_game_won_updates game in
          Deferred.return result))
;;

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
     print_endline "Server Started";
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
