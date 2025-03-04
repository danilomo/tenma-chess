(ns tenma-chess.actor
  (:import  [org.apache.pekko.pattern Patterns]
            [pekko_clj.actor FnWrapper])
  (:require
   [clojure.edn :refer [read-string]]
   [integrant.core :as ig]
   [manifold.stream :as s]
   [pekko-clj.core :as p]
   [tenma-chess.chess.core :as chess :refer [new-game make-move]]
   [tenma-chess.algebraic :as algebraic :refer [make-move-algebraic]]))

(def ^:dynamic *timeout* 30000)

;;;;;;;;;; player actor

(defn player-actor [this m]
  (println (str this " " m " " (type m)))
  (let [[type-msg msg] m
        parent (-> this (.getContext) (.getParent))
        callback (:callback @this)]
    (case type-msg
      :move (when (:my-turn @this)
              (.tell this parent msg))
      :status (do
                (callback m)
                (when (= :ok msg)
                  (assoc @this :my-turn false)))
      :game-start (do
                    (callback m)
                    nil)
      :your-turn (do
                   (callback m)
                   (assoc @this :my-turn true)))))

;;;;;;;; game actor

(defn game-running [this move]
  (let [{white :white
         black :black
         game :game} @this
        turn (:turn game)
        updated-game (make-move game move)
        current-p (if (even? turn) white black)
        next-p (if (even? turn) black white)]
    (if (nil? updated-game)
      (.tell this current-p [:status :not-ok])
      (do
        (.tell this current-p [:status :ok])
        (.tell this next-p [:your-turn move])
        (assoc @this :game updated-game)))))

(defn new-player-actor [game-actor color callback]
  (.spawn game-actor player-actor {:color color
                                   :my-turn (= :white color)
                                   :callback callback}))

(defn game-init [this]
  (let [{white-ref :white-ref
         black-ref :black-ref
         white-cb :white-cb
         black-cb :black-cb} @this
        white (new-player-actor this :white white-cb)
        black (new-player-actor this :black black-cb)]
    (.tell this white-ref white)
    (.tell this black-ref black)
    (.tell this white [:game-start :white])
    (.tell this black [:game-start :black])
    {:white white :black black :game (new-game)}))

(defn game-lobby [this callback]
  (if (= :none @this)
    {:white-ref (.getSender this)
     :white-cb callback}
    (do
      (.spawn this {:function game-running
                    :pre-start game-init
                    :state (merge @this {:black-ref (.getSender this)
                                         :black-cb callback})})
      :none)))

;;;;;;;;;;;;;;; definitions

(defn- join-game [system actor in-stream]
  (let [out-stream (s/stream)
        callback #(s/put! in-stream (pr-str %))]
    (.onComplete
     (Patterns/ask actor callback *timeout*)
     (FnWrapper/create #(let [actor (.get %)]
                          (s/consume
                           (fn [move]
                             (.tell actor (read-string move) nil))
                           out-stream)))
     (.getDispatcher system))
    out-stream))

(defprotocol GameServer
  (join [_ in-stream])
  (close [_]))

(defrecord PekkoGameServer [system game-lobby]
  GameServer
  (join [_ in-stream]
    (join-game system game-lobby in-stream))
  (close [_]
    (.terminate system)))

(defmethod ig/init-key :chess/server [_ _]
  (let [system (p/actor-system)
        game-lobby (p/new-actor system game-lobby :none)]
    (PekkoGameServer. system game-lobby)))

(defmethod ig/halt-key! :chess/server [_ server]
  (close server))
