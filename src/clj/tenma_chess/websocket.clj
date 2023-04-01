(ns tenma-chess.websocket
  (:require
   [clojure.core.async :as a :refer [<!! >!! <! >!]]
   [tenma-chess.chess :refer :all]
   [tenma-chess.concurrent :refer :all]
   [aleph.http :as http]
   [manifold.bus :as bus]
   [manifold.deferred :as d]
   [manifold.stream :as s]
   [reitit.ring :as ring]
   [ring.middleware.params :as params]))

(def non-websocket-request
  {:status 400
   :headers {"content-type" "application/text"}
   :body "Expected a websocket request."})

(defn echo-handler
  "This handler sets up a websocket connection, and then proceeds to echo back every message
   it receives from the client.  The value yielded by `websocket-connection` is a **duplex
   stream**, which represents communication to and from the client.  Therefore, all we need to
   do in order to echo the messages is connect the stream to itself.
   Since any request it gets may not be a valid handshake for a websocket request, we need to
   handle that case appropriately."
  [req]
  (if-let [socket (try
                    @(http/websocket-connection req)
                    (catch Exception e
                      nil))]
    (s/connect socket socket)
    non-websocket-request))

(defn echo-handler
  "The previous handler blocks until the websocket handshake completes, which unnecessarily
   takes up a thread.  This accomplishes the same as above, but asynchronously. "
  [req]
  (-> (http/websocket-connection req)
      (d/chain
       (fn [socket]
         (s/connect socket socket)))
      (d/catch
       (fn [_]
         non-websocket-request))))

(defn echo-handler
  "This is another asynchronous handler, but uses `let-flow` instead of `chain` to define the
   handler in a way that at least somewhat resembles the synchronous handler."
  [req]
  (->
   (d/let-flow [socket (http/websocket-connection req)]
               (s/connect socket socket))
   (d/catch
    (fn [_]
      non-websocket-request))))

;; to represent all the different chat rooms, we use an **event bus**, which is simple
;; implementation of the publish/subscribe model
(def chatrooms (bus/event-bus))

(defn chat-handler
  [req]
  (d/let-flow [conn (d/catch
                     (http/websocket-connection req)
                     (fn [_] nil))]

              (if-not conn

                ;; if it wasn't a valid websocket handshake, return an error
                non-websocket-request

                ;; otherwise, take the first two messages, which give us the chatroom and name
                (d/let-flow [room (s/take! conn)
                             name (s/take! conn)]

                            ;; take all messages from the chatroom, and feed them to the client
                            (s/connect
                             (bus/subscribe chatrooms room)
                             conn)

                            ;; take all messages from the client, prepend the name, and publish it to the room
                            (s/consume
                             #(bus/publish! chatrooms room %)
                             (->> conn
                                  (s/map #(str name ": " %))
                                  (s/buffer 100)))

                            ;; A ring handler expects some sort of HTTP response, so just give it `nil`
                            nil))))

(def server-chan (start-game-server! make-move-edn))

(defn handle-connection [input output]
  (let [chan-in (a/chan 100)
        chan-out (a/chan 100)]
    (a/go
      ;(>! output "Hello, meu nego")
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
            (do
              (let [move (<! chan-out)]
                (>! output (str move))
                (recur true)))))))))

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

(def handler
  (params/wrap-params
   (ring/ring-handler
    (ring/router
     [["/echo" echo-handler]
      ["/chat" chat-handler]])
    (ring/create-default-handler))))
