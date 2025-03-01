(ns tenma-chess.websocket
  (:require
   [manifold.deferred :as d]
   [tenma-chess.actor :refer [join-game game-waiting-players]]
   [aleph.http :as http]
   [manifold.stream :as s]
   [pekko-clj.core :as p]))

(def non-websocket-request
  {:status 400
   :headers {"content-type" "application/text"}
   :body "Expected a websocket request."})

(def sys (p/actor-system))

(def game (p/new-actor sys game-waiting-players :none))

(defn chess-handler [_]
  (fn [req]
    (d/let-flow [stream (d/catch (http/websocket-connection req) (fn [_] nil))
                 player-in (s/stream)
                 player-out (join-game game player-in)]
      (s/connect player-in stream)
      (s/connect stream player-out)
      nil)))
