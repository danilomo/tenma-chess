(ns tenma-chess.chess.core
  (:require [clojure.edn :as edn]
            [tenma-chess.chess.utils :refer [get-color get-p get-pieces kings opposite-color]]
            [tenma-chess.chess.moves :refer [BK WK available-moves-map castling-positions]]
            [tenma-chess.chess.pgn   :refer [translate-move-to-pgn]]))

(def initial-board
  [[:br :bn :bb :bq :bk :bb :bn :br]
   [:bp :bp :bp :bp :bp :bp :bp :bp]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [:wp :wp :wp :wp :wp :wp :wp :wp]
   [:wr :wn :wb :wq :wk :wb :wn :wr]])

(def piece-map {:wk \u2654
                :wq \u2655
                :wr \u2656
                :wb \u2657
                :wn \u2658
                :wp \u2659
                :bk \u265A
                :bq \u265B
                :br \u265C
                :bb \u265D
                :bn \u265E
                :bp \u265F})

(defn board-pieces [board]
  (let [pieces (->> (for [i (range 0 8) j (range 0 8)]
                      (let [p (get-in board [i j])]
                        (if p [p [i j]] nil)))
                    (filter some?))]
    (group-by (fn [[piece _pos]] (get-color piece)) pieces)))

(def game {:board initial-board
           :turn 0
           :moves []
           :captured-pieces []
           :pieces (board-pieces initial-board)
           :kings {:black [0 4] :white [7 4]}
           :castling-info #{}})

(defn turn-color [turn]
  (if (even? turn) :white :black))

(defn available-moves
  [game pos]
  (let [piece (get-p game pos)
        [i j] pos
        moves (available-moves-map piece)]
    (if
     piece (moves game i j)
     #{})))

(defn available-moves-for-turn
  [{turn :turn :as game} pos]
  (let [piece (get-p game pos)
        color-p (get-color piece)]
    (cond
      (nil? pos) #{}
      (not= (turn-color turn) color-p) #{}
      :else (available-moves game pos))))

(defn update-castling-info [{castling-info :castling-info} position]
  (if (and (castling-positions position) (not (castling-info position)))
    (conj castling-info position)
    castling-info))

(defn simple-move [{board :board :as game} [src-i src-j] [dst-i dst-j]]
  (let [origin-p (get-p game src-i src-j)
        dest-p (get-p game dst-i dst-j)
        new-board (-> board
                      (assoc-in [src-i src-j] nil)
                      (assoc-in [dst-i dst-j] origin-p))]
    [dest-p new-board]))

(defn right-castling-move [{board :board :as game} src [i j]]
  (let [p (get-p game src)
        color (get-color p)]
    (when (and (or (= p :bk) (= p :wk))
               (or (= src WK) (= src BK))
               (= j 6))
      [nil (-> board
               (assoc-in [i 4] nil)
               (assoc-in [i 7] nil)
               (assoc-in [i 6] (if (= color :white) :wk :bk))
               (assoc-in [i 5] (if (= color :white) :wr :br))) :right-castling])))

(defn left-castling-move [{board :board :as game} src [i j]]
  (let [p (get-p game src)
        color (get-color p)]
    (when (and (or (= p :bk) (= p :wk))
               (or (= src WK) (= src BK))
               (= j 2))
      [nil (-> board
               (assoc-in [i 4] nil)
               (assoc-in [i 0] nil)
               (assoc-in [i 2] (if (= color :white) :wk :bk))
               (assoc-in [i 3] (if (= color :white) :wr :br))) :left-castling])))

(defn en-passant-move [{board :board :as game} [src-i src-j] [i j]]
  (let [p (get-p game src-i src-j)
        dst-p (get-p game i j)
        color (get-color p)
        captured (if (= color :white) :bp :wp)]
    (when (and (or (= p :wp) (= p :bp)) (nil? dst-p) (not= src-j j))
      [captured (-> board
                    (assoc-in [src-i src-j] nil)
                    (assoc-in [i j] p)
                    (assoc-in [src-i j] nil)) :en-passant])))

(defn apply-move-to-board [game src dst]
  (or (en-passant-move game src dst)
      (right-castling-move game src dst)
      (left-castling-move game src dst)
      (simple-move game src dst)))

(declare apply-move-to-game)

(defn threats-to-king [game color]
  (->> (get-pieces game (opposite-color color))
       (map (fn [[_p pos]] [pos (available-moves game pos)]))
       (filter (fn [[_pos moves]] (and (not-empty moves)
                                       (moves (get-in game [:kings color])))))))

(defn valid-moves-for-piece [game src-pos]
  (let [piece (get-p game src-pos)
        color (get-color piece)]
    (set (->> (available-moves-for-turn game src-pos)
              (map (fn [pos] [(apply-move-to-game game src-pos pos) pos]))
              (map (fn [[g pos]] [(count (threats-to-king g color)) pos]))
              (filter (fn [[c _pos]] (= c 0)))
              (map (fn [[_c pos]] pos))))))

(defn list-valid-moves [{:keys [turn pieces] :as game}]
  (let [color-for-turn (if (even? turn) :white :black)
        positions (map second (color-for-turn pieces))]
    (into {} (map (fn [pos] [pos (valid-moves-for-piece game pos)]) positions))))

(defn apply-promotion [game {:keys [src dst promotion]} new-board]
  (let [piece (get-p game src)
        turn (:turn game)
        [dst-i _dst-j] dst
        is-pawn (#{:bp :wp} piece)
        last-rank (if (even? turn) (= 0 dst-i) (= 7 dst-i))]
    (if (and is-pawn last-rank)
      (assoc-in new-board dst promotion)
      new-board)))

(defn apply-move-to-game
  ([{:keys [board turn captured-pieces] :as game} {:keys [src dst] :as move}]
   (let [[captured new-board event] (apply-move-to-board game src dst)
         new-board (apply-promotion game move new-board)
         board-pieces (board-pieces new-board)
         new-turn (inc turn)
         new-game (merge game {:previous-board board
                               :board new-board
                               :turn new-turn
                               :kings (kings {:board new-board})
                               :pieces board-pieces
                               :captured-pieces (if captured
                                                  (conj captured-pieces captured)
                                                  captured-pieces)
                               :last-captured captured
                               :last-event event
                               :castling-info (update-castling-info game src)})]
     new-game))
  ([game src dst]
   (apply-move-to-game game {:src src :dst dst})))

(defn valid-move? [g {:keys [src dst]}]
  (let [available-moves (or (get-in g [:valid-moves src]) #{})]
    (available-moves dst)))

(defn make-move [game move]
  (when (valid-move? game move)
    (let [new-game (apply-move-to-game game move)
          color (if (even? (:turn new-game)) :white :black)
          check (seq (threats-to-king new-game color))
          valid-moves (list-valid-moves new-game)
          move-count (reduce + (map #(count (second %)) valid-moves))
          check-mate (and check (= 0 move-count))
          move-pgn (translate-move-to-pgn {:last-event (:last-event new-game)
                                           :last-captured (:last-captured new-game)
                                           :move move
                                           :game game
                                           :check check
                                           :check-mate check-mate})
          game-over (when check-mate true) ; (or checkmate draw surrender...
          outcome (when check-mate (if (= :black color) "1-0" "0-1"))]
      (merge new-game {:valid-moves valid-moves
                       :check check
                       :check-mate check-mate
                       :stale-mate (and (not check) (= 0 move-count))
                       :move-count move-count
                       :game-over game-over
                       :outcome outcome
                       :moves (conj (:moves game) move-pgn)}))))

(defn make-move-edn [game move-str]
  (let [move (edn/read-string move-str)]
    (make-move game move)))

(defn new-game []
  (merge game {:valid-moves (list-valid-moves game)}))

(defn find-moves [{valid-moves :valid-moves} src]
  (or (get valid-moves src) #{}))

