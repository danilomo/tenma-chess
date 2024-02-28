(ns tenma-chess.chess.utils)

(def whites #{:wr :wn :wb :wq :wk :wp})

(defn not-contains? [collection elem] (not (contains? collection elem)))

(defn invalid-or-nil? [piece] (or (nil? piece) (= piece :invalid)))

(defn get-pieces [game color]
  (get-in game [:pieces color]))

(defn opposite-color [color] (if (= color :white) :black :white))

(defn get-p
  "Get the contents of the board in the position (i,j). A piece symbol (:wr, :wn, etc.), nil (free position),
   or :invalid if (i,j) is out of the bounds of the board"
  ([game [i j]] (get-p game i j))
  ([game i j]
   (if (and (< i 8) (< j 8) (>= i 0) (>= j 0))
     (get-in game [:board i j])
     :invalid)))

(defn get-color
  ([piece] (if (contains? whites piece) :white :black))
  ([game pos] (let [piece (get-p game pos)] (get-color piece))))

(defn kings [game]
  (into {} (filter some? (for [i (range 0 8) j (range 0 8)]
                           (let [p (get-p game i j)]
                             (when (or (= :wk p) (= :bk p))
                               [(get-color p) [i j]]))))))

(defn if-free
  "Returns a tuple [i j] if the position (i,j) is free, otherwise returns nil"
  ([game [i j]] (if-free game i j))
  ([game i j]
   (when (nil? (get-p game i j))
     [i j])))

(defn capture
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


