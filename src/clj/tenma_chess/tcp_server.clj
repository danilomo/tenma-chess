(ns tenma-chess.tcp-server
  (:require
   [tenma-chess.concurrent :refer [start-game-server! handle-connection!]]
   [clojure.core.async :as a :refer [>!! <! >!]]))

(defn socket-reader [inputstream]
  (java.io.BufferedReader. (java.io.InputStreamReader. inputstream)))

(defn socket-writer [outputstream]
  (java.io.OutputStreamWriter. outputstream))

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
    [input-chan output-chan]))

(defn start-server! [port]
  (let [connections (atom [])
        server-socket (java.net.ServerSocket. port)
        {server-chan :channel games :games} (start-game-server!)
        runnable (fn []
                   (loop []
                     (when-not (.isClosed server-socket)
                       (let [socket (try (.accept server-socket)
                                         (catch Exception e nil))
                             _ (when socket (swap! connections conj socket))
                             [input output] (if socket
                                              (socket-connection socket)
                                              [nil nil])]
                         (when socket
                           (future (handle-connection! input output server-chan))
                           (recur))))))
        t (Thread. runnable)]
    (.start t)
    {:server-socket server-socket :connections connections :games games}))

(defn stop-server! [{:keys [server-socket connections]}]
  (doseq [conn @connections]
    (try (.close conn)))
  (reset! connections nil)
  (.close server-socket))


