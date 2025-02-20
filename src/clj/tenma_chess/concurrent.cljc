(ns tenma-chess.concurrent
  (:require
   [integrant.core :as ig]
   [tenma-chess.chess.core :as chess :refer [make-move-edn new-game]]
   [tenma-chess.algebraic :as algebraic :refer [make-move-algebraic]]
   [clojure.core.async :as a :refer [close! <! >! >!! <!!]]))

(defn- close-game! [{{w-in :in w-out :out} :white {b-in :in b-out :out} :black}]
  (a/close! w-in)
  (a/close! b-in)
  (a/close! w-out)
  (a/close! b-out))

(defn- start-game-match!
  "Implements the process of a running chess match between two concurrent users"
  [game-match move-func on-close!]
  (a/go-loop [game @(:game game-match)]
    (let [[player opponent] (if (even? (:turn game)) [:white :black] [:black :white])
          chan-in (get-in game-match [player :in])
          chan-out-player (get-in game-match [player :out])
          chan-out-opponent (get-in game-match [opponent :out])
          move (<! chan-in)
          new-game (move-func game move)]
      (cond
        (nil? move) (do
                      (>! chan-out-opponent {:type :opponent-disconnected})
                      (on-close!)
                      (println "Jogo acabou, circulando")
                      (close-game! game-match))
        (nil? new-game) (do
                          (println "Puta que pariu!!!")
                          (>! chan-out-player {:valid false})
                          (recur game))
        :else (do
                (>! chan-out-player {:valid true})
                (>! chan-out-opponent {:move move})
                (reset! (:game game-match) new-game)
                (if-not (:game-over new-game)
                  (recur new-game)
                  (do
                    (on-close!)
                    (println "Jogo acabou, circulando")
                    (close-game! game-match))))))))

(def id-gen (atom 0))

(defn- join-game!
  "Implements the match-making logic"
  [{:keys [waiting game-map move-func] :as games} player chan-server]
  (if (empty? waiting)
    (assoc games :waiting (conj waiting {:white player}))
    (let [game-atom (atom (new-game))
          new-waiting (rest waiting)
          first-waiting (first waiting)
          white (:white first-waiting)
          match (merge
                 first-waiting
                 {:game game-atom :black player})]
      (a/go
        (>! (:out player) {:type :start :color :black})
        (>! (:out white) {:type :start :color :white}))
      (let [id (swap! id-gen inc)
            on-close! (fn [] (a/go (>! chan-server {:type :game-over :id id})))]
        (start-game-match! match move-func on-close!)
        (merge games {:waiting new-waiting
                      :game-map (assoc game-map
                                       id
                                       match)})))))

(defn- stats-message! [game {chan-resp :channel}]
  (let [games (or game {})
        response {:waiting-players (count (or (:waiting games) []))
                  :active-games (count (or (:game-map games) []))}]
    (a/go
      (>! chan-resp response))
    games))

(defn- stop-message! [game]
  (merge {:stopped true} game))

(defn- game-over! [{game-map :game-map :as game} {id :id}]
  (merge game {:game-map (dissoc game-map id)}))

(defn- handle-message! [game {type :type :as message} chan-in]
  (println (str "->" chan-in))
  (case type
    :stats (stats-message! game message)
    :stop (stop-message! game)
    :game-over (game-over! game message)
    (join-game! game message chan-in)))

(defn start-game-server!
  "Start a go process that runs forever waiting for game-join requests"
  ([] (start-game-server! make-move-algebraic))
  ([move-func] (let [chan-in (a/chan)
                     games (atom {:waiting [] :game-map {} :move-func move-func})]
                 (a/go (loop []
                         (let [_ (println "Esperando por alguma coisa")
                               msg (<! chan-in)]
                           (swap! games handle-message! msg chan-in)
                           (if-not (:stopped @games)
                             (recur)
                             (do
                               (close! chan-in)
                               (reset! games nil)
                               (println "Min dê, papai"))))))
                 {:channel chan-in :games games})))

(defn handle-connection!
  "Provides the bridge between the concurrent game process and an underlying networking implementation 
  (e.g. aleph, java.net.Socket, netty, etc.)"
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
                  _ (>! output (str {valid :valid}))
                  _ (println (str "Move was okay? " valid))]
              (if valid
                (recur false)
                (recur true)))
            (let [move (<! chan-out)]
              (>! output (str move))
              (recur true))))))))

(defn new-player [] {:in (a/chan) :out (a/chan)})

(defn get-stats [game]
  (let [c (a/chan)]
    (>!! (:channel game) {:type :stats :channel c})
    (<!! c)))

(defmethod ig/init-key :chess/server [_ {:keys [format]}]
  (let [make-move-func (case format :pgn make-move-algebraic make-move-edn)
        server (start-game-server! make-move-func)]
    (println (str "Started chess server with func" make-move-func))
    (:channel server)))

(defmethod ig/halt-key! :chess/server [_ server]
  (println "Trying to stop chess server")
  (>!! server {:type :stop}))
