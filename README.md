# jane-street-sweets
### By: Ruth Taddesse and Nakib Abedin

#### Project Overview
A web-based game inspired by Pit, where players blindly trade commodities in an attempt to corner the market on a certain good. 

#### Launch Instructions 
0. Clone this repository and cd into it
1. Install all dependcies by doing `opam install .`
2. Build our project by doing `dune build`
3. Get the server up and running by doing `dune exec ./bin/server.exe -- start-game -port 10000`. Note that the port number can be different but you must call the same port from the client side.
4. Join the game from the client side by running `dune exec ./bin/client.exe -- join-game -name "name" -players 3 -host "Hostname of the server" -port 10000`. Players represents the number of players you want in your game. Hostname and port must match the hostname and port of the server. Name represents your name.
5. Have Fun!


### How it Works:
#### Client Side
Let's say you want to play a game of Pigge with n players. You're going to get placed into the waiting lobby for the game until n players have queued up for your game. Once enough players have queued up, the game will automatically start. 
On the client side, our UI is built of the ncurses library in OCaml. Thus, you have multiple windows that display information. On the left hand side is your book window, which shows a live representation of all of the orders that are currently open. On the top right, you can see the current commodities that you have in your hand. On the bottom right, you get move up and down using the `w` key to move up and `d` key to move down. To select an option, press the enter key. To reset your current selections state, press the esc button. 

Keep battling it out till there is a winner (or many) and once the game ends, you will be notified on your screen.

#### Server Side
Our server can support multiple simultaneous games going on at the same time. It simply needs clients to specify the number of players that they want to play with and also their name. 
Our server has a log of all the trades that are currently going on, and it might be fun for a moderator to see these messages as the trades are happening.


