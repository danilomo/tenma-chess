(ns tenma-chess.algebraic-test
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [tenma-chess.algebraic :refer [make-move-algebraic parse-pgn]]
            [tenma-chess.chess :refer [new-game]]
            [clojure.test :refer [deftest is testing run-test]]))

(def games-list (map
                 #(str "games/" %)
                 ;["Bobby Fischer - My 60 Memorable Games (8).pgn"]))
                 (clojure.string/split (slurp (io/resource "games.txt")) #"\n")))

(def multi-games-list (map
                       #(str "multi-game-files/" %)
                       (clojure.string/split (slurp (io/resource "multi-game-files.txt")) #"\n")))

(defn check-game-algebraic [game-as-pgn]
  (let [history (->> game-as-pgn
                     (:moves)
                     (reduce (fn [games move]
                               (let [last-g (:game (last games))]
                                 (conj games {:game (let [new-g (make-move-algebraic last-g move)]
                                                      (is new-g
                                                          (str "Invalid move " move " - " (:meta-inf game-as-pgn)))
                                                      new-g)
                                              :move move})))
                             [{:game (new-game)}]))
        outcome (last history)]
    (doseq [entry history]
      (let [{move :move game :game} entry
            dst (:destination move)
            captured-piece (get-in (:previous-board game) dst)]
        (when (and (:last-captured game) (not (:type move)) (not= (:last-event game) :en-passant))
          (is
           (= (:last-captured game) captured-piece)
           (str "Testing capture for move " move)))

        (when (= "O-O" (:move move))
          (is
           (= :right-castling (:last-event game))
           (str "Testing right castle for move " move)))

        (when (= "O-O-O" (:move move))
          (is
           (= :left-castling (:last-event game))
           (str "Testing left castle for move " move)))

        (when (= :en-passant (:last-event game))
          (is
           (or (= :bp (:last-captured game)) (= :wp (:last-captured game)))
           (str "Testing en passant " move)))

        (when (:check move)
          (is (:check game)
              (str "Testing check in move " move)))

        (when (:check-mate move)
          (println (str "Checkmate " move))
          (is (:check-mate game)
              (str "Testing check in move " move)))))

    (if-not (= "true" (get-in game-as-pgn [:meta-inf :Ignore]))
      (let [expected-moves (into [] (map :move (:moves game-as-pgn)))
            moves-history (get-in outcome [:game :moves])]
        (doseq [move (map vector expected-moves moves-history)]
          (let [[expected actual] move]
            (is (= expected actual) "Testing generation of algebraic notation")))))))

(deftest test-games-algebraic
  (doseq [file-name games-list]
    (let [file-contents (slurp (clojure.java.io/resource file-name))
          game (parse-pgn file-contents)]
      (println (str "Testing game: " file-name))
      (testing (str "Testing game: " file-name) (check-game-algebraic game)))))

(defn do-multi-game-pgn [callback file-name]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource file-name))]
    (loop [[lines acc] [(line-seq rdr) []]]
      (let [line (first lines)
            remainder (rest lines)]
        (if line
          (cond (= "" (s/trim line)) (recur [remainder acc])
                (and (nil? acc) (.startsWith line "[Event ")) (recur [remainder (conj acc line)])
                (.startsWith line "[Event ") (do (callback (s/join "\n" acc)) (recur [remainder [line]]))
                :else (recur [remainder (conj acc line)]))
          (callback (s/join "\n" acc)))))))

(deftest test-multigames-file
  (doseq [file-name  multi-games-list]
    (println (str "---> Processing file " file-name))
    (letfn [(callback [game]
              (when (not= "" (s/trim game))
                (let [g (parse-pgn game)]
                  (println (str "Testing game " (get-in g [:meta-inf :Event])
                                " - " (get-in g [:meta-inf :White])
                                " - " (get-in g [:meta-inf :Black])))
                  (check-game-algebraic g))))]
      (do-multi-game-pgn callback file-name))))
