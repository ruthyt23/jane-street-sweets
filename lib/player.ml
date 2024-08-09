open! Async
open! Core
module Player_id = Int

type t =
  { player_id : Player_id.t
  ; player_name : string
  ; mutable hand : Commodity.t list
  }
[@@deriving equal, sexp_of, bin_io]

(* let update_hand = 0 ;; *)

let create_player player_id player_name =
  { player_id; player_name; hand = [] }
;;

let hand_to_string (hand : Commodity.t list) =
  let commodity_freq = Hashtbl.create (module Commodity) in
  List.iter hand ~f:(fun commodity ->
    match Hashtbl.find commodity_freq commodity with
    | Some count ->
      Hashtbl.set commodity_freq ~key:commodity ~data:(count + 1)
    | None -> Hashtbl.set commodity_freq ~key:commodity ~data:1);
  let commodity_str_list =
    List.map (Hashtbl.keys commodity_freq) ~f:(fun commodity ->
      sprintf
        "(%s  %d) "
        (Commodity.to_string commodity)
        (Hashtbl.find_exn commodity_freq commodity))
  in
  String.concat ~sep:" " commodity_str_list
;;

let print_hand t =
  printf "Hand for player %d: " t.player_id;
  List.iter t.hand ~f:(fun commodity ->
    printf "%s  " (Commodity.to_string commodity))
;;
