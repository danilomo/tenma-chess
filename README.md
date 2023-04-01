# tenma-chess
A remote chess game powered by Clojure

## About Tenma Chess

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

## Acknowledgements 

Creating Tenma Chess was possible thanks to the open game databases from lichess.org and chess.com (used in the test resources)
