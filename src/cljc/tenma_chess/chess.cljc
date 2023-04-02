(ns tenma-chess.chess
  (:require [clojure.edn :as edn]))

;; definitions

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

(def whites #{:wr :wn :wb :wq :wk :wp})

(def LEFT-WR [7 0])

(def RIGHT-WR [7 7])

(def LEFT-BR [0 0])

(def RIGHT-BR [0 7])

(def WK [7 4])

(def BK [0 4])

(def castling-positions #{LEFT-WR RIGHT-WR WK LEFT-BR RIGHT-BR BK})

(def initial-board
  (into []
        (concat
         [[:br :bn :bb :bq :bk :bb :bn :br]]
         [(into [] (repeat 8 :bp))]
         (map (fn [_row] (vec (repeat 8 nil))) (range 0 4))
         [(into [] (repeat 8 :wp))]
         [[:wr :wn :wb :wq :wk :wb :wn :wr]])))

;; miscelaneous helper functions and variables

(defn not-contains? [collection elem] (not (contains? collection elem)))

(defn invalid-or-nil? [piece] (or (nil? piece) (= piece :invalid)))

(defn get-color [piece] (if (contains? whites piece) :white :black))

(defn get-pieces [game color]
  (get-in game [:pieces color]))

(defn opposite-color [color] (if (= color :white) :black :white))

(defn board-pieces [board]
  (let [pieces (->> (for [i (range 0 8) j (range 0 8)]
                      (let [p (get-in board [i j])]
                        (if p [p [i j]] nil)))
                    (filter some?))]
    (group-by (fn [[piece _pos]] (get-color piece)) pieces)))

(def game {:board initial-board
           :turn 0
           :captured-pieces []
           :pieces (board-pieces initial-board)
           :kings {:black [0 4] :white [7 4]}
           :castling-info #{}})

(defn get-p
  "Get the contents of the board in the position (i,j). A piece symbol (:wr, :wn, etc.), nil (free position),
   or :invalid if (i,j) is out of the bounds of the board"
  ([game [i j]] (get-p game i j))
  ([game i j]
   (if (and (< i 8) (< j 8) (>= i 0) (>= j 0))
     (get-in game [:board i j])
     :invalid)))

(defn kings [game]
  (into {} (filter some? (for [i (range 0 8) j (range 0 8)]
                           (let [p (get-p game i j)]
                             (when (or (= :wk p) (= :bk p))
                               [(get-color p) [i j]]))))))

(defn- if-free
  "Returns a tuple [i j] if the position (i,j) is free, otherwise returns nil"
  ([game [i j]] (if-free game i j))
  ([game i j]
   (when (nil? (get-p game i j))
     [i j])))

(defn- capture
  "Returns a tuple [i j] if the position (i,j) in the game board contains a piece
  of opposite color than the color argument, otherwise returns nil"
  [game color i j]
  (let
   [piece (get-p game i j)
    dest-color (get-color piece)]
    (when (and
           (not (invalid-or-nil? piece))
           (not= color dest-color))
      [i j])))

(defn- free-or-capture [game color i j]
  (or (if-free game i j) (capture game color i j)))

(defn- moves 
  "Helper function that gets a list of variadic arguments and transform them into
  a set of moves, discarding nil elements"
  [& values]
  (into #{} (filter some? values)))

(defn path
  "Returns a sequence of free positions (int tuples) in the chess board
  starting from an initial position, in straight line, until it reaches:
  - the board border
  - a piece of same color as the piece positioned in the initial position
  - a piece of different color for capture, which means the position of this
    piece is included in the sequence

  Arguments:
  - game: the game object
  - color: the color of the piece in the initial position
  - func: the function that tells the next piece (i.e. it defines the direction up, down, left, up-left, etc._

  Returns:
  - a vector of int tuples" 
  [game color func [i j]]
  (let [sequence (into []
                       (take-while some? (map
                                          #(if-free game %)
                                          (drop 1 (iterate func [i j])))))
        last-piece-pos (if (empty? sequence) (func [i j]) (func (last sequence)))
        last-piece (get-p game last-piece-pos)
        last-p-color (get-color last-piece)]
    (cond
      (= last-piece :invalid) sequence
      (= color last-p-color) sequence
      :else (conj sequence last-piece-pos))))

;; move functions for each piece type

(defn knight-moves
  [game i j]
  (let [color (get-color (get-p game i j))]
    (moves
     (free-or-capture game color (inc i) (+ j 2))
     (free-or-capture game color (inc i) (- j 2))
     (free-or-capture game color (dec i) (+ j 2))
     (free-or-capture game color (dec i) (- j 2))
     (free-or-capture game color (+ i 2) (inc j))
     (free-or-capture game color (+ i 2) (dec j))
     (free-or-capture game color (- i 2) (inc j))
     (free-or-capture game color (- i 2) (dec j)))))

(defn right-castling [{:keys [castling-info board]} color i j]
  (let [original-pos (if (= :white color) WK BK)
        right-rook (if (= :white color) RIGHT-WR RIGHT-BR)
        row (get board i)]
    (when (and (= [i j] original-pos)
               (not-contains? castling-info original-pos)
               (not-contains? castling-info right-rook)
               (and (nil? (get row 5)) (nil? (get row 6))))
      [i 6])))

(defn left-castling [{:keys [castling-info board]} color i j]
  (let [original-pos (if (= :white color) WK BK)
        right-rook (if (= :white color) LEFT-WR LEFT-BR)
        row (get board i)]
    (when (and (= [i j] original-pos)
               (not-contains? castling-info original-pos)
               (not-contains? castling-info right-rook)
               (and (nil? (get row 1)) (nil? (get row 2)) (nil? (get row 3))))
      [i 2])))

(defn king-moves
  [game i j]
  (let [color (get-color (get-p game i j))]
    (moves
     (right-castling  game color i j)
     (left-castling   game color i j)
     (free-or-capture game color (inc i) (inc j))
     (free-or-capture game color (inc i) (dec j))
     (free-or-capture game color (dec i) (inc j))
     (free-or-capture game color (dec i) (dec j))
     (free-or-capture game color i (inc j))
     (free-or-capture game color i (dec j))
     (free-or-capture game color (inc i) j)
     (free-or-capture game color (dec i) j))))

(defn en-passant [{:keys [previous-board] :as game} color i j position]
  (let [row (if (= color :white) 3 4)
        target-i (if (= color :white) 2 5)
        initial-i-enemy (if (= color :white) 1 6)
        enemy-pawn-type (if (= color :white) :bp :wp)
        target-j (position j)]
    (when (and (= i row)
               (= enemy-pawn-type (get-p game i target-j))
               (= enemy-pawn-type (get-p {:board previous-board} initial-i-enemy target-j)))
      [target-i target-j])))

(defn white-pawn-moves
  [game i j]
  (moves
   (en-passant game :white i j inc)
   (en-passant game :white i j dec)
   (when (and (= :wp (get-p game i j)) (= 6 i) (nil? (get-p game 5 j)) (nil? (get-p game 4 j))) [(- i 2) j])
   (if-free game (dec i) j)
   (capture game :white (dec i) (inc j))
   (capture game :white (dec i) (dec j))))

(defn black-pawn-moves
  [game i j]
  (moves
   (en-passant game :black i j inc)
   (en-passant game :black i j dec)
   (when (and (= :bp (get-p game i j)) (= 1 i) (nil? (get-p game 2 j)) (nil? (get-p game 3 j))) [(+ i 2) j])
   (if-free game (inc i) j)
   (capture game :black (inc i) (inc j))
   (capture game :black (inc i) (dec j))))

(defn bishop-moves
  [game i j]
  (let [color (get-color (get-p game i j))]
    (into #{}
          (concat
           (path game color (fn [[i j]] [(dec i) (dec j)]) [i j])
           (path game color (fn [[i j]] [(dec i) (inc j)]) [i j])
           (path game color (fn [[i j]] [(inc i) (dec j)]) [i j])
           (path game color (fn [[i j]] [(inc i) (inc j)]) [i j])))))

(defn rook-moves
  [game i j]
  (let [color (get-color (get-p game i j))]
    (into #{}
          (concat
           (path game color (fn [[i j]] [i (dec j)]) [i j])
           (path game color (fn [[i j]] [i (inc j)]) [i j])
           (path game color (fn [[i j]] [(inc i) j]) [i j])
           (path game color (fn [[i j]] [(dec i) j]) [i j])))))

(defn queen-moves
  [game i j]
  (into (bishop-moves game i j) (rook-moves game i j)))

(def available-moves-map
  {:wk king-moves
   :wq queen-moves
   :wr rook-moves
   :wb bishop-moves
   :wn knight-moves
   :wp white-pawn-moves
   :bk king-moves
   :bq queen-moves
   :br rook-moves
   :bb bishop-moves
   :bn knight-moves
   :bp black-pawn-moves})

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

(defn make-move [game move]
  (let [new-game (apply-move-to-game game move)
        color (if (even? (:turn new-game)) :white :black)
        check (seq (threats-to-king new-game color))
        valid-moves (list-valid-moves new-game)
        move-count (reduce + (map #(count (second %)) valid-moves))]
    (merge new-game {:valid-moves valid-moves
                     :check check
                     :check-mate (and check (= 0 move-count))
                     :stale-mate (and (not check) (= 0 move-count))
                     :move-count move-count})))

(defn make-move-edn [game move-str]
  (let [move (edn/read-string move-str)]
    (make-move game move)))

(defn new-game []
  (merge game {:valid-moves (list-valid-moves game)}))

(defn find-moves [{valid-moves :valid-moves} src]
  (or (get valid-moves src) #{}))

