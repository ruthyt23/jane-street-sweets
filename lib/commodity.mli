open! Async
open! Core
open! Fzf

type t

val game_commodities : int -> t list
val to_string : t -> string
val of_string : string -> t
