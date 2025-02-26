(ns tenma-chess.actor
  (:import  [org.apache.pekko.pattern Patterns]
            [pekko_clj.actor FnWrapper])
  (:require
   [aleph.tcp :as tcp]
   [gloss.core :as gloss]
   [gloss.io :as io]
   [manifold.stream :as s]
   [pekko-clj.core :as p]
   [tenma-chess.utils :refer [print-game]]
   [tenma-chess.chess.core :as chess :refer [new-game]]
   [tenma-chess.algebraic :as algebraic :refer [make-move-algebraic]]))

(def sys (p/actor-system))

(def ^:dynamic *timeout* 1000)

(defn join-game [actor in-stream]
  (let [out-stream (s/stream)
        callback #(s/put! in-stream %)]
    (.onComplete
     (Patterns/ask actor callback *timeout*)
     (FnWrapper/create #(let [actor (.get %)]
                          (s/consume
                           (fn [move]
                             (.tell actor [:move move] nil))
                           out-stream)))
     (.getDispatcher sys))
    out-stream))

;;;;;;;;;; player actor

(defn player-actor [this m]
  (println  (str this " " m " " @this "\n\n\n"))
  (let [[type-msg msg] m
        callback (:callback @this)]
    (case type-msg
      :move (when (:my-turn @this)
              (.tell this (:game-actor @this) msg))
      :status (if (= :ok msg)
                (do
                  (callback "ok")
                  (assoc @this :my-turn false))
                (do
                  (callback "Invalid move.")
                  nil))
      :game-start (do
                    (callback (str "Game started. You play as " msg))
                    nil)
      :your-turn (do
                   (callback (str "Your turn. Opponent played " msg))
                   (assoc @this :my-turn true)))))

;;;;;;;; game actor

(defn game-running [this move]
  (let [{white :white
         black :black
         game :game} @this
        turn (:turn game)
        updated-game (make-move-algebraic game move)
        current-p (if (even? turn) white black)
        next-p (if (even? turn) black white)]
    (println (str (.getSender this) " - " move " - " (nil? updated-game)))
    (if (nil? updated-game)
      (.tell this current-p [:status :not-ok])
      (do
        (println (print-game updated-game))
        (.tell this current-p [:status :ok])
        (.tell this next-p [:your-turn move])
        (assoc @this :game updated-game)))))

(defn new-player-actor [game-actor color callback]
  (.spawn game-actor player-actor {:color color
                                   :my-turn (= :white color)
                                   :game-actor (.getSelf game-actor)
                                   :callback callback}))

(defn game-waiting-players [this callback]
  (if (not (:white @this))
    (let [actor (new-player-actor this :white callback)]
      (.reply this actor)
      (assoc @this :white actor))
    (let [actor (new-player-actor this :black callback)]
      (.reply this actor)
      (.tell this (:white @this) [:game-start :white])
      (.tell this actor [:game-start :black])
      [game-running (merge @this {:game (new-game)
                                  :black actor})])))

;;;;;;;;;;;;;;; definitions

(def game (p/new-actor sys game-waiting-players {}))

(def protocol (gloss/string :utf-8 :delimiters ["\n" "\r\n"]))

(defn chess-handler [stream _]
  (let [player-in (s/stream)
        player-out (join-game game player-in)]
    (s/connect (s/map #(io/encode protocol %) player-in) stream)
    (s/connect (io/decode-stream stream protocol) player-out)
    stream))

(defn start-server [] (tcp/start-server chess-handler {:port 8080}))

