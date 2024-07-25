open! Core
open! Async
open! Fzf

(* Generates player id for next queued player *)
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

  (* To make sure our memeory doesn't grow unbounded, maybe add a feature to
     cleanup games that have already finished *)
  let clean_up () = 0
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

let game_data_handle (_client : unit) (query : Rpcs.Game_state.Query.t) =
  let game_opt =
    Queue.find game_manager.games_that_have_started ~f:(fun game ->
      equal game.game_id query)
  in
  match game_opt with
  | Some game ->
    let winning_players = Game.check_for_wins game in
    (match Int.(List.length winning_players >= 1) with
     | true -> return (Rpcs.Game_state.Response.Game_over winning_players)
     | false ->
       (* let player_hand = Game.get_hand_for_player game query in *)
       return Rpcs.Game_state.Response.In_progress)
  | None -> Deferred.return Rpcs.Game_state.Response.Waiting
;;

let player_game_data_handle
  (_client : unit)
  (query : Rpcs.Player_game_data.Query.t)
  =
  let game_opt =
    Queue.find game_manager.games_that_have_started ~f:(fun game ->
      equal game.game_id query.game_id)
  in
  (* print_s [%message (game_manager.games_that_have_started : Game.t
     Queue.t)]; *)
  match game_opt with
  | None ->
    return
      { Rpcs.Player_game_data.Response.current_book = []; player_hand = [] }
  | Some game ->
    let list_of_trade_amounts = Hashtbl.keys game.open_trades in
    let response_book =
      List.map list_of_trade_amounts ~f:(fun amount_to_trade ->
        let _, commodity =
          Hashtbl.find_exn game.open_trades amount_to_trade
        in
        commodity, amount_to_trade)
    in
    let player_hand = Game.get_hand_for_player game query.player_id in
    (* print_s [%message (player_hand : Commodity.t list)]; *)
    return
      { Rpcs.Player_game_data.Response.current_book = response_book
      ; player_hand
      }
;;

let make_trade_handle (_client : unit) (query : Rpcs.Make_trade.Query.t) =
  printf
    "Player %d is trying to trade %d of commodity %s\n"
    query.player_id
    query.quantity
    (Commodity.to_string query.commodity);
  let game_opt =
    Queue.find game_manager.games_that_have_started ~f:(fun game ->
      equal game.game_id query.game_id)
  in
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
       print_endline "Order placed in book"
     | Rpcs.Make_trade.Response.Trade_rejected msg -> print_endline msg
     | Rpcs.Make_trade.Response.Trade_successful ->
       print_endline "Order successful");
    Deferred.return result
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ Rpc.Rpc.implement Rpcs.Waiting_room.rpc waiting_handle
      ; Rpc.Rpc.implement Rpcs.Game_state.rpc game_data_handle
      ; Rpc.Rpc.implement Rpcs.Player_game_data.rpc player_game_data_handle
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
