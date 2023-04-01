(ns tenma-chess.tcp-server 
  (:require 
   [tenma-chess.concurrent :refer [start-game-server!]] 
   [clojure.core.async :as a :refer [>!! <! >!]]))

; TODO: 
; * Remove handle-connection from this file
; * Adjust concurrent/handle-connection so it can be used here and by websocket.clj
; * Add command line argument in the main method so we can start the http or the tcp server via command line

(defn socket-reader [inputstream]
  (new java.io.BufferedReader (new java.io.InputStreamReader inputstream)))

(defn socket-writer [outputstream]
  (new java.io.OutputStreamWriter outputstream))

(defn socket-connection [socket]
  (let [reader (-> socket
                   (.getInputStream)
                   (socket-reader))
        writer (-> socket
                   (.getOutputStream)
                   (socket-writer))
        input-chan (a/chan)
        output-chan (a/chan)]
    (a/go (loop []
            (let [reader-chan (a/chan)]
              (future (let [move (.trim (.readLine reader))]
                        (>!! reader-chan move)))
              (let [move (<! reader-chan)]
                (>! input-chan move)
                (recur)))))
    (a/go (loop []
            (let [msg (<! output-chan)]
              (.write writer msg)
              (.flush writer)
              (recur))))
    {:input input-chan :output output-chan}))

(defn handle-connection [socket server-chan]
  (let [{input :input output :output} (socket-connection socket)
        chan-in (a/chan 100)
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

(defn start-server []
  (let [server-socket (new java.net.ServerSocket 1243)
        server-chan (start-game-server!)]
    (loop []
      (let [socket (.accept server-socket)]
        (future (handle-connection socket server-chan))
        (recur)))))