open! Core

module Commodity = struct
  type t =
    | Fairlife
    | Pocky
    | Water
  [@@deriving equal, enumerate]

end

module Player = struct
  type t =
    { player_id : int
    ; mutable hand : Commodity.t list
    } [@@deriving equal]
end

module Game_State = struct
  type t =
    | In_progress
    | Game_over of { winner : Player.t option }
  [@@deriving equal]
end

type t =
  { players : Player.t list
  ; game_state : Game_State.t
  }

(* let generate_hand  *)

(* ;; *)

(* let start_game = 




;;

let trade_cards = 
;;

let print_card = 
;; *)

(* let command =
  Command.group
    ~summary:"Driver"
    [ "start", start_game; "trade", trade_cards; "hand", print_hand ]
;; *)
