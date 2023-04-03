(ns tenma-chess.websocket
  (:require
   [clojure.core.async :as a :refer [<! >!]]
   [tenma-chess.chess :refer [make-move-edn]]
   [tenma-chess.concurrent :refer [start-game-server!]]
   [aleph.http :as http]
   [manifold.deferred :as d]
   [manifold.stream :as s]))

(def non-websocket-request
  {:status 400
   :headers {"content-type" "application/text"}
   :body "Expected a websocket request."})

(def server-chan (start-game-server! make-move-edn))

(defn handle-connection [input output]
  (let [chan-in (a/chan 100)
        chan-out (a/chan 100)]
    (a/go
      (>! server-chan {:in chan-in :out chan-out})
      (let [start-msg (<! chan-out)
            color (:color start-msg)
            my-turn (= :white color)]
        (>! output (str start-msg))
        (loop [turn my-turn]
          (if turn
            (let [move (<! input)]
              (>! chan-in move)
              (recur false))
            (let [move (<! chan-out)]
              (>! output (str move))
              (recur true))))))))

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
                  (handle-connection input output)
                  nil))))
