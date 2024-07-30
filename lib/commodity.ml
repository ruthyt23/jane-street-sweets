open! Async
open! Core
open! Fzf

type t =
  | Fairlife
  | Pocky
  | Water
  | Boba
  | Mango
  | Starburst
  | Hint
  | Gum
  | Kitkat
[@@deriving equal, enumerate, compare, sexp_of, hash, bin_io]

(* game_commodities --> generates the n commodities that will be traded in
   the game based on n players *)
let game_commodities num_players =
  let commodity_array = Array.of_list all in
  Array.permute commodity_array;
  List.init num_players ~f:(fun index -> Array.get commodity_array index)
;;

let to_string t =
  match t with
  | Fairlife -> "Fairlife"
  | Pocky -> "Pocky"
  | Water -> "Water"
  | Boba -> "Boba"
  | Mango -> "Mango"
  | Starburst -> "Starburst"
  | Hint -> "Hint"
  | Gum -> "Gum"
  | Kitkat -> "Kitkat"
;;

let of_string str =
  match str with
  | "Fairlife" -> Some Fairlife
  | "Pocky" -> Some Pocky
  | "Water" -> Some Water
  | "Boba" -> Some Boba
  | "Mango" -> Some Mango
  | "Starburst" -> Some Starburst
  | "Hint" -> Some Hint
  | "Gum" -> Some Gum
  | "Kitkat" -> Some Kitkat
  | _ -> None
;;
