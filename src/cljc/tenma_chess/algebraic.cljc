(ns tenma-chess.algebraic
  (:require
   [tenma-chess.chess :as chess :refer [get-p make-move]]
   [clojure.edn :as edn]
   [clojure.string :as s :refer [split]]))

(def type-of-piece {:wk \K
                    :wq \Q
                    :wr \R
                    :wb \B
                    :wn \N
                    :wp \P
                    :bk \K
                    :bq \Q
                    :br \R
                    :bb \B
                    :bn \N
                    :bp \P})

(def piece-map [{\K :wk \Q :wq \R :wr \B :wb \N :wn \P :wp}
                {\K :bk \Q :bq \R :br \B :bb \N :bn \P :bp}])

(defn get-piece-from-type [piece turn]
  (get-in piece-map [(mod turn 2) piece]))

(def EIGHT 56)
(def A 97)

(def to-i {\8 0, \7 1, \6 2, \5 3, \4 4, \3 5, \2 6, \1 7})

(def to-j {\a 0, \b 1, \c 2, \d 3, \e 4, \f 5, \g 6, \h 7})

(defn simple [move]
  (let [[piece file rank] (vec move)]
    {:piece piece,
     :destination [(to-i rank) (to-j file)]}))

(defn simple-pawn [move]
  (let [[file rank] (vec move)]
    {:piece \P,
     :destination [(to-i rank) (to-j file)]}))

(defn simple-pawn-capture [move]
  (let [[src-file file rank] (vec move)]
    {:piece \P,
     :src-j (to-j src-file),
     :destination [(to-i rank) (to-j file)]}))

(defn disambiguating-rank [move]
  (let [[piece src-rank file rank] (vec move)]
    {:piece piece,
     :destination [(to-i rank) (to-j file)]
     :src-i (to-i src-rank)}))

(defn disambiguating-file [move]
  (let [[piece src-file file rank] (vec move)]
    {:piece piece,
     :destination [(to-i rank) (to-j file)]
     :src-j (to-j src-file)}))

(defn disambiguating-both [move]
  (let [[piece src-file src-rank file rank] (vec move)]
    {:piece piece,
     :destination [(to-i rank) (to-j file)]
     :src-i (to-i src-rank)
     :src-j (to-j src-file)}))

(defn pawn-promotion [move]
  (let [[file rank _ piece] (vec move)]
    {:piece \P,
     :destination [(to-i rank) (to-j file)]
     :promotion piece}))

(defn pawn-promotion-with-capture [move]
  (let [[src-file file rank _ piece] (vec move)]
    {:piece \P,
     :src-j (to-j src-file),
     :destination [(to-i rank) (to-j file)]
     :promotion piece}))

(defn left-castling [_]
  {:type :left-castling})

(defn right-castling [_]
  {:type :right-castling})

(def notations
  [[#"[R|N|B|Q|K]x?[a-h][1-8][+]?[#]?" simple]
   [#"[a-h][1-8][+]?[#]?" simple-pawn]
   [#"[a-h]x[a-h][1-8][+]?[#]?" simple-pawn-capture]
   [#"[R|N|B|Q|K][1-8]x?[a-h][1-8][+]?[#]?" disambiguating-rank]
   [#"[R|N|B|Q|K][a-h]x?[a-h][1-8][+]?[#]?" disambiguating-file]
   [#"[R|N|B|Q|K]x?[a-h][1-8]x?[a-h][1-8][+]?[#]?" disambiguating-both]
   [#"[a-h][1-8][=][R|N|B|Q|K][+]?[#]?" pawn-promotion]
   [#"[a-h]x[a-h][1-8][=][R|N|B|Q|K][+]?[#]?" pawn-promotion-with-capture]
   [#"O[-]O[-]O[+]?" left-castling]
   [#"O[-]O[+]?" right-castling]
   [#"0[-]1" (fn [_] {:type :white-wins})]
   [#"1[-]0" (fn [_] {:type :black-wins})]
   [#"1/2[-]1/2" (fn [_] {:type :draw})]
   [#".*" (fn [move] {:type :invalid :move move})]])

(defn notation [move]
  (let [parser-f (first (->> notations
                             (map (fn [[reg notation]] (when (re-matches reg move) notation)))
                             (filter some?)))
        parsed-move (when parser-f (parser-f (s/replace move "x" "")))]
    (when parsed-move (-> parsed-move
                          (assoc :move move)
                          (assoc :check (or (s/includes? move "#") (s/includes? move "+")))
                          (assoc :check-mate (or (s/includes? move "++") (s/includes? move "#")))))))

(defn create-filter [{:keys [piece destination src-i src-j]}]
  (fn [game [i j]]
    (let [p (get-p game i j)
          type-p (type-of-piece p)]
      (and (if (some? src-i) (= i src-i) true)
           (if (some? src-j) (= j src-j) true)
           (= type-p piece)
           ((or (get-in game [:valid-moves [i j]]) #{}) destination)))))

(defn make-simple-move [game move]
  (let [promotion (get-piece-from-type (:promotion move) (:turn game))
        pred (when move (create-filter move))
        dst (when move (:destination move))
        positions (filter some? (for [i (range 0 8)
                                      j (range 0 8)]
                                  (when (get-p game i j) [i j])))
        filtered (filter #(pred game %) positions)
        src (first filtered)]
    (when src (make-move game {:src src :dst dst :promotion promotion}))))

(defn make-castling [game type]
  (let [turn (:turn game)
        src-i (if (even? turn) 7 0)
        src-j 4
        dst-i src-i
        dst-j (if (= type :left-castling) 2 6)]
    (make-move game {:src [src-i src-j] :dst [dst-i dst-j]})))

(defn make-move-algebraic [game move]
  (let [move (if (string? move)
               (notation move)
               move)
        type (:type move)]
    (cond (= :left-castling type) (make-castling game type)
          (= :right-castling type) (make-castling game type)
          (= :black-wins type) game
          (= :white-wins type) game
          (= :draw type) game
          :else (make-simple-move game move))))

(defn parse-moves [line]
  (let [parts (filter #(not (re-matches #"\d+[.]" %)) (split line #"[ ]+"))
        new-moves (map (fn [part] (notation part)) parts)]
    new-moves))

(defn parse-pgn [text]
  (let [lines (filter #(not= "" %) (map #(.trim %) (s/split text #"\n")))
        parse-status (fn [status] (let [[k v] (edn/read-string status)]
                                    [(keyword k) v]))
        reduce-f (fn [{:keys [meta-inf moves] :as acc} elem]
                   (cond
                     (.startsWith elem "[") (assoc acc :meta-inf (conj meta-inf (parse-status elem)))
                     :else (assoc acc :moves (conj moves elem))))

        parsed-initial (reduce reduce-f {:moves [] :meta-inf []} lines)
        moves (s/join " " (:moves parsed-initial))]
    (merge parsed-initial {:moves (-> moves
                                      (s/replace #"\{\[[^\[\]\{\}]*\]\}" "")
                                      (s/replace #"[$]\d+" "")
                                      (parse-moves))
                           :meta-inf (into {} (:meta-inf parsed-initial))})))




