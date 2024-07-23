open! Async
open! Core
open! Fzf

type t =
  { player_id : int
  ; mutable hand : Commodity.t list
  }
[@@deriving equal, sexp_of, bin_io]

val print_hand : t -> unit
