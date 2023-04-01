# tenma-chess
A remote chess game powered by Clojure

## About Tenma Chess

Tenma Chess is an experimental chess server that runs as a reagent application connected by the game server via websockets. It runs on top of:

* core/async
* Aleph+ring+manifold
* Reagent

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

