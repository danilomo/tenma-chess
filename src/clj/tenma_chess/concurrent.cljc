(ns tenma-chess.concurrent
  (:require
   [tenma-chess.chess.core :as chess :refer [new-game]]
   [tenma-chess.utils :as utils :refer [print-game]]
   [tenma-chess.algebraic :as algebraic :refer [make-move-algebraic]]
   [clojure.core.async :as a :refer [<! >!]]))

(defn close-game! [{{w-in :in w-out :out} :white {b-in :in b-out :out} :black}]
  (a/close! w-in)
  (a/close! b-in)
  (a/close! w-out)
  (a/close! b-out))

(defn start-game-match!
  "Implements the process of a running chess match between two concurrent users"
  [game-match move-func]
  (a/go-loop [game @(:game game-match)]
    (let [[player opponent] (if (even? (:turn game)) [:white :black] [:black :white])
          chan-in (get-in game-match [player :in])
          chan-out-player (get-in game-match [player :out])
          chan-out-opponent (get-in game-match [opponent :out])
          move (<! chan-in)
          new-game (move-func game move)]
      (if (nil? new-game)
        (do (>! chan-out-player {:valid false})
            (recur game))
        (do
          (>! chan-out-player {:valid true})
          (>! chan-out-opponent {:move move})
          (reset! (:game game-match) new-game)
          (if-not (:game-over new-game)
            (recur new-game)
            (do
              (println "Jogo acabou, circulando")
              (close-game! game-match))))))))

(def id-gen (atom 0))

(defn join-game!
  "Implements the match-making logic"
  [{:keys [waiting game-map move-func] :as games} player]
  (if (empty? waiting)
    (assoc games :waiting (conj waiting {:white player}))
    (let [game-atom (atom (new-game))
          new-waiting (rest waiting)
          first-waiting (first waiting)
          white (:white first-waiting)
          match (merge
                 first-waiting
                 {:game game-atom :black player})]
      (add-watch game-atom :print-board
                 (fn [_ _ _ neu]
                   (println (str "Board\n" (print-game neu)))))
      (a/go
        (>! (:out player) {:type :start :color :black})
        (>! (:out white) {:type :start :color :white}))
      (start-game-match! match move-func)
      (merge games {:waiting new-waiting
                    :game-map (assoc game-map
                                     (swap! id-gen inc)
                                     match)}))))

(defn start-game-server!
  "Start a go process that runs forever waiting for game-join requests"
  ([] (start-game-server! make-move-algebraic))
  ([move-func] (let [chan-in (a/chan)
                     games (atom {:waiting [] :game-map {} :move-func move-func})]
                 (a/go (loop []
                         (let [msg (<! chan-in)]
                           (swap! games join-game! msg)
                           (recur))))
                 {:channel chan-in :games games})))

(defn handle-connection!
  "Provides the bridge between the concurrent game process and an underlying networking implementation (e.g. aleph, java.net.Socket, netty, etc.)"
  [input output server-chan]
  (let [chan-in (a/chan)
        chan-out (a/chan)]
    (a/go
      (>! server-chan {:in chan-in :out chan-out})
      (let [start-msg (<! chan-out)
            color (:color start-msg)
            my-turn (= :white color)]
        (>! output (str start-msg))
        (loop [turn my-turn]
          (if turn
            (let [move (<! input)
                  _ (println (str "Got move: " move))
                  _ (>! chan-in move)
                  {valid :valid} (<! chan-out)
                  _ (println (str "Move was okay? " valid))]
              (if valid
                (recur false)
                (recur true)))
            (let [move (<! chan-out)]
              (>! output (str move))
              (recur true))))))))

