(ns tenma-chess.concurrent
  (:require
   [tenma-chess.chess :as chess :refer [new-game]]
   [tenma-chess.utils :as utils :refer [print-game]]
   [tenma-chess.algebraic :as algebraic :refer [make-move-algebraic]]
   [clojure.core.async :as a :refer [<! >!]]))

(defn start-game-match! [game-match move-func]
  (a/go (loop [game @(:game game-match)]
          (let [chan-in-sel (if (even? (:turn game)) [:white :in] [:black :in])
                chan-out-sel (if (odd? (:turn game)) [:white :out] [:black :out])
                chan-in (get-in game-match chan-in-sel)
                chan-out (get-in game-match chan-out-sel)
                move (<! chan-in)
                new-game (move-func game move)]
            (if (nil? new-game)
              (do
                (recur game))
              (do
                (>! chan-out {:move move})
                (reset! (:game game-match) new-game)
                (recur new-game)))))))

(defn start-match
  ([move-func] (let [game-atom (atom (new-game))
                     game-match {:game game-atom
                                 :white {:in (a/chan) :out (a/chan 100)}
                                 :black {:in (a/chan) :out (a/chan 100)}}]
                 (add-watch game-atom :print-board
                            (fn [k a alt neu]
                              (println (str "Board\n" (print-game neu)))))

                 (start-match game-match move-func)))
  ([game-match move-func] (do (start-game-match! game-match move-func)
                              game-match)))

(def id-gen (atom 0))

(defn join-game! [{:keys [waiting game-map move-func] :as games} player]
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
                 (fn [k a alt neu]
                   (println (str "Board\n" (print-game neu)))))
      (a/go
        (>! (:out player) {:type :start :color :black})
        (>! (:out white) {:type :start :color :white}))
      (start-match match move-func)
      (merge games {:waiting new-waiting
                    :game-map (assoc game-map
                                     (swap! id-gen inc)
                                     match)}))))

(defn start-game-server!
  ([] (start-game-server! make-move-algebraic))

  ([move-func] (let [chan-in (a/chan)
                     games (atom {:waiting [] :game-map {} :move-func move-func})]
                 (a/go (loop []
                         (let [msg (<! chan-in)]
                           (swap! games join-game! msg)
                           (recur))))
                 chan-in)))
