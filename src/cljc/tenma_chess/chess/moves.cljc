(ns tenma-chess.chess.moves
  (:require [tenma-chess.chess.utils :refer :all]))

(def LEFT-WR [7 0])

(def RIGHT-WR [7 7])

(def LEFT-BR [0 0])

(def RIGHT-BR [0 7])

(def WK [7 4])

(def BK [0 4])

(def castling-positions #{LEFT-WR RIGHT-WR WK LEFT-BR RIGHT-BR BK})
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




