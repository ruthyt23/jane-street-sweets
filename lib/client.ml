open! Core
open! Async
open! Fzf

let (current_hand : Commodity.t list ref) = ref []

let print_hand hand =
  print_endline "Current hand: ";
  List.iter hand ~f:(fun commodity ->
    printf "%s  " (Commodity.to_string commodity))
;;

let successful_trade hand =
  Core.print_endline "Trade successful!";
  current_hand := hand
;;

let handle_trade ~conn ~player_id ~commodity ~num_cards =
  let query =
    { Rpcs.Make_trade.Query.player_id
    ; Rpcs.Make_trade.Query.commodity
    ; Rpcs.Make_trade.Query.quantity = num_cards
    }
  in
  let%bind (response : Rpcs.Make_trade.Response.t) =
    Rpc.Rpc.dispatch_exn Rpcs.Make_trade.rpc conn query
  in
  (match response with
   | Trade_successful -> ()
   | In_book ->
     Core.print_endline "No matching trade found - offer placed on book"
   | Trade_rejected message -> Core.print_endline message);
  return ()
;;

let make_trades ~conn ~player_id ~hand =
  print_hand hand;
  let commodities_string_list =
    List.dedup_and_sort hand ~compare:Commodity.compare
    |> List.map ~f:(fun commodity -> Commodity.to_string commodity)
  in
  let num_cards_string_list =
    List.init 4 ~f:(fun num -> Int.to_string (num + 1))
  in
  print_endline "\nWhat commodity would you like to trade? ";
  let selection_for_commodities = Pick_from.inputs commodities_string_list in
  let%bind commodity_opt =
    pick_one
      selection_for_commodities
      ~prompt_at_top:()
      ~height:(List.length commodities_string_list + 3)
  in
  match commodity_opt |> ok_exn with
  | None -> return ()
  | Some commodity ->
    print_endline "How many would you like to trade? (1-4)";
    let selection_for_numcards = Pick_from.inputs num_cards_string_list in
    let%bind num_cards_opt =
      pick_one selection_for_numcards ~prompt_at_top:() ~height:7
    in
    (match num_cards_opt |> ok_exn with
     | None -> return ()
     | Some cards ->
       let num_cards = Int.of_string cards in
       handle_trade
         ~conn
         ~player_id
         ~commodity:(Commodity.of_string commodity)
         ~num_cards)
;;

let game_over winner_list =
  if List.length winner_list = 1
  then Core.print_endline "GAME OVER! WINNER: "
  else Core.print_endline "GAME OVER! WINNERS: ";
  List.iter winner_list ~f:(fun ((player : Player.t), commodity) ->
    Core.print_endline
      [%string "%{player.player_name#String} with %{commodity#Commodity}"])
;;

let pull_game_data ~conn ~player_id =
  Deferred.repeat_until_finished () (fun _ ->
    let (query : Rpcs.Game_data.Query.t) = player_id in
    let%bind (response : Rpcs.Game_data.Response.t) =
      Rpc.Rpc.dispatch_exn Rpcs.Game_data.rpc conn query
    in
    match response with
    | Rpcs.Game_data.Response.In_progress hand ->
      if not
           (List.is_empty !current_hand
            && List.equal Commodity.equal !current_hand hand)
      then successful_trade hand;
      make_trades ~conn ~player_id ~hand |> Deferred.value_exn;
      return (`Repeat ())
    | Rpcs.Game_data.Response.Game_over winner_list ->
      game_over winner_list;
      return (`Finished ())
    | Rpcs.Game_data.Response.Waiting -> return (`Repeat ()))
;;

let connect_to_server =
  Command.async
    ~summary:"Join game"
    (let%map_open.Command () = return ()
     and name = flag "-name" (required string) ~doc:"_ name of player"
     and int_port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       let port = Host_and_port.create ~host:name ~port:int_port in
       let join_game_query = name in
       let conn =
         Rpc.Connection.client (Tcp.Where_to_connect.of_host_and_port port)
         |> Deferred.value_exn
         |> Result.ok_exn
       in
       let%bind (player_id : Rpcs.Waiting_room.Response.t) =
         Rpc.Rpc.dispatch_exn Rpcs.Waiting_room.rpc conn join_game_query
       in
       pull_game_data ~conn ~player_id)
;;

let command =
  Command.group ~summary:"Pit Player" [ "join-game", connect_to_server ]
;;
