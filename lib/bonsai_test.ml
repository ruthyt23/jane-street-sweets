open! Core
open Bonsai
open Virtual_dom

let name = "y/n"
let number = 1

let double_num = 

let message_vdom ~name ~number =
  Vdom.Node.div
    ~attrs:[ [%css {|font-size: 16px;|}] ]
    [ Vdom.Node.textf "Hello, %s! Your current number is %d" name number ]
;;

let double_number_button =
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.on_click (fun _ -> double_num) ]
    [ Vdom.Node.text "Double your number!" ]
;;

let number_state ~name (local_ graph) = 
;;

let app (local_ graph) = number_state ~name:(Bonsai.return name) graph

let () = Bonsai_web.Start.start app