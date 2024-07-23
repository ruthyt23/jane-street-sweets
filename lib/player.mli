open! Async
open! Core
open! Fzf

type t =
  { player_id : int
  ; mutable hand : Commodity.t list
  }
[@@deriving equal]

val print_hand : t -> unit
