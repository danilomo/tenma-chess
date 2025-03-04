(ns tenma-chess.websocket
  (:require
   [manifold.deferred :as d]
   [aleph.http :as http]
   [manifold.stream :as s]
   [tenma-chess.actor :refer [join]]))

(def non-websocket-request
  {:status 400
   :headers {"content-type" "application/text"}
   :body "Expected a websocket request."})


(defn chess-handler [chess-server]
  (fn [req]
    (d/let-flow [stream (d/catch (http/websocket-connection req) (fn [_] nil))
                 player-in (s/stream)
                 player-out (join chess-server  player-in)]
                (s/connect player-in stream)
                (s/connect stream player-out)
                nil)))
