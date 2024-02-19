(ns tenma-chess.websocket
  (:require
   [clojure.core.async :as a :refer [<! >!]]
   [tenma-chess.chess.core :refer [make-move-edn]]
   [tenma-chess.concurrent :refer [start-game-server! handle-connection!]]
   [aleph.http :as http]
   [manifold.deferred :as d]
   [manifold.stream :as s]))

(def non-websocket-request
  {:status 400
   :headers {"content-type" "application/text"}
   :body "Expected a websocket request."})

(def game-server (start-game-server! make-move-edn))

(defn chess-handler
  [req]
  (d/let-flow [conn (d/catch
                     (http/websocket-connection req)
                     (fn [_] nil))]
              (if-not conn
                non-websocket-request
                (let [input (a/chan)
                      output (a/chan)]
                  (s/connect conn input)
                  (s/connect output conn)
                  (handle-connection! input output (:channel game-server))
                  nil))))
