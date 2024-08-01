open! Async
open! Core
open! Fzf

type t =
  { player_id : int
  ; player_name : string
  ; mutable hand : Commodity.t list
  }
[@@deriving equal, sexp_of, bin_io]

(* let update_hand = 0 ;; *)

let create_player player_id player_name =
  { player_id; player_name; hand = [] }
;;

let hand_to_string (hand : Commodity.t list) =
  let commodity_str_list =
    List.map hand ~f:(fun commodity -> Commodity.to_string commodity)
  in
  String.concat ~sep:" " commodity_str_list
;;

let print_hand t =
  printf "Hand for player %d: " t.player_id;
  List.iter t.hand ~f:(fun commodity ->
    printf "%s  " (Commodity.to_string commodity))
;;
