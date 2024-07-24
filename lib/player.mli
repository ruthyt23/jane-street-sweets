open! Async
open! Core
open! Fzf

type t =
  { player_id : int
  ; player_name : string
  ; mutable hand : Commodity.t list
  }
[@@deriving equal, sexp_of, bin_io]

val print_hand : t -> unit
