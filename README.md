# tenma-chess
A remote chess game powered by Clojure

## Intro

Tenma Chess is an experimental chess server that runs as a reagent application connected by the game server via websockets. It runs on top of:

* core/async
* Aleph+ring+manifold
* Reagent

## Project Organization 

* cljs - contains the Reagent application
* cljc - contains the "chess" and "algebraic" namespace for the core game logic and parsing of algebraic notation (files and moves). This implementation is used both by frontend and the game server
* clj - contains the server implementation (ring + aleph) and the "concurrent" namespace which implements the game server logic as CSP (go blocks)

## Instructions

### Running tests

```
lein test
```

### Installing shadow-cljs and react modules:

```
npm install --save-dev shadow-cljs
npm i
```

### Launching tenma chess

```
npx shadow-cljs watch app (for the reagent application, running on port 3000)
lein with-profile dev,server run (for the websocket server application, running on port 8080)
```

### To-do/About Tenma Chess

Tenma Chess started as a programming challenge I gave myself to learn Clojure. The initial goal was just to write the chess logic and a command line chess game. At some point I started to have many thoughts "What if I add this"/"What if I add that", and started to further expand the initial goal: added the PGN (portable game notation parser), the Reagent application, and finally the game server.

In the "todo.org" file I collect thoughts about what I could do to extend the project and put it in a good shape. No the aim is to make a "lichess.org lite", a website which can host online games between anonymous and registered users.

The current source code is not well organized (may lack cohesion) and the game server is full of flaws and resource/memory leaks (channels and connection aren't closed, timeouts are not checked). The game logic was tested with dozen of games, but still lacks draw detection (which can be more complicated the whole game itself).

## Acknowledgements 

Creating Tenma Chess was possible thanks to the open game databases from lichess.org and chess.com (used in the test resources)
