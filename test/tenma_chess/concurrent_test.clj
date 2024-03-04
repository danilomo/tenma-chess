(ns tenma-chess.concurrent-test
  (:require [clojure.string :as s]
            [clojure.core.async :as a :refer [timeout >!! <!! <! >! go go-loop]]
            [clojure.java.io :as io]
            [tenma-chess.algebraic :refer [make-move-algebraic parse-pgn]]
            [tenma-chess.chess.core :refer [new-game make-move]]
            [tenma-chess.concurrent :refer :all]
            [clojure.test :refer [deftest is testing run-test run-tests]]))

(def pgn-game "[Event \"World Senior Teams +50\"]
[Site \"Struga MKD\"]
[Date \"2023.09.26\"]
[Round \"7.1\"]
[White \"Adams,Mi\"]
[Black \"Dishman,S\"]
[Result \"1-0\"]
[WhiteElo \"2662\"]
[BlackElo \"2304\"]
[ECO \"B27\"]

1.e4 c5 2.Nf3 g6 3.c3 d5 4.e5 Nc6 5.Bb5 Bg4 6.d4 cxd4 7.cxd4 Qb6 8.Bxc6+ bxc6
9.Nbd2 Qb5 10.Qe2 e6 11.Qxb5 cxb5 12.Nh4 Ne7 13.h3 Bf5 14.Nxf5 Nxf5 15.Nf3 h6
16.Bd2 Kd7 17.Ke2 a5 18.Rhc1 Bg7 19.Rc5 Rhb8 20.Rac1 Rb7 21.g4 Ne7 22.Ne1 Bf8
23.Nd3 Ra6 24.R5c2 Nc6 25.Be3 a4 26.f4 b4 27.b3 axb3 28.axb3 Na7 29.f5 gxf5
30.gxf5 Nb5 31.Rc8 Nc3+ 32.R1xc3 bxc3 33.Rxf8 Ra2+ 34.Kf3 Ke7 35.Bxh6 exf5
36.Rc8 c2 37.Nc5 Rb4 38.Bf8+")

(defn moves-list [pgn-game] (map :move (:moves (parse-pgn pgn-game))))

(defn moves-list-to-match [moves]
  {:white (take-nth 2 moves)
   :black (take-nth 2 (rest moves))})

(defn start-player! [player moves color]
  (let [{in :in out :out} player]
    (go
      (<! out)
      (when (= color :black) (<! out))
      (loop [m-list moves]
        (println (str color " played " (first m-list)))
        (>! in (first m-list))
        (println (str "Response " (<! out)))
        (let [response (<! out)]
          (if (not-empty (rest m-list))
            (recur (rest m-list))
            response))))))

(def match (moves-list-to-match (moves-list pgn-game)))

(deftest test-async-game
  (let [match (moves-list-to-match (moves-list pgn-game))
        game-server (start-game-server!)
        chan-server (:channel game-server)
        games (:games game-server)
        p1 (new-player)
        p2 (new-player)
        result-p1 (start-player!  p1 (:white match) :white)
        result-p2 (start-player!  p2 (:black match) :black)]
    (go
      (>! chan-server p1)
      (>! chan-server p2))
    (println (<!! result-p1))
    (println (<!! result-p2))
    (is (every? nil? [(<!! (:in p1))
                      (<!! (:out p1))
                      (<!! (:in p2))
                      (<!! (:out p2))])
        "Asserts channels are closed")
    (let [stats (get-stats game-server)]
      (is (= {:waiting-players 0 :active-games 0} stats) "assert no waiting-games in the queue")
      (println game-server))))
