open! Async
open! Core
open! Fzf

type t =
  { player_id : int
  ; mutable hand : Commodity.t list
  }
[@@deriving equal, sexp_of, bin_io]

(* let update_hand = 0 ;; *)

let print_hand t =
  printf "Hand for player %d: " t.player_id;
  List.iter t.hand ~f:(fun commodity ->
    printf "%s  " (Commodity.to_string commodity))
;;
