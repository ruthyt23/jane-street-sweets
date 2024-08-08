open! Core
open! Async
open! Fzf
open Curses

module Input : sig
  type t =
    | Up
    | Down
    | Escape
    | Enter
    | Invalid

  val of_int : int -> t
end

module State_manager : sig
  module Current_state : sig
    type t =
      | Selecting_Commodity
      | Selecting_Quantity
    [@@deriving equal]
  end

  type t =
    { mutable hand_cursor : int
    ; mutable quantity_cursor : int
    ; mutable hand_selection_list : Commodity.t list
    ; quantity_selection_list : int list
    ; mutable state : Current_state.t
    ; mutable curr_commodity_selected : Commodity.t option
    ; mutable curr_quantity_selected : int option
    }

  val init : unit -> t
  val reset : t -> unit
  val move_up : t -> unit
  val move_down : t -> unit
  val handle_enter : t -> unit
  val handle_escape : t -> unit
end

type t =
  { parent_window : window
  ; height : int
  ; width : int
  ; trade_parent_window : window
  ; commodity_window : window
  ; quantity_window : window
  ; hand_window : window
  ; book_update_window : window
  ; trade_update_window : window
  ; state_manager : State_manager.t
  }

val acs_codes : Acs.acs
val nice_box : window -> unit
val clear : window -> unit
