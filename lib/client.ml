open! Core
open! Async
open! Fzf

let command = Command.group ~summary:"Pit Player" [ "join-game", join_game ]
