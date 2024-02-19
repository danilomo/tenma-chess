(ns tenma-chess.chess.pgn
  (:require [tenma-chess.chess.utils :refer :all]))

(def piece-to-letter {:wk \K
                      :wq \Q
                      :wr \R
                      :wb \B
                      :wn \N
                      :bk \K
                      :bq \Q
                      :br \R
                      :bb \B
                      :bn \N})

(def i-to-rank [\8 \7 \6 \5 \4 \3 \2 \1])

(def j-to-file [\a \b \c \d \e \f \g \h])

(defn disambiguate-move [game piece {src :src dst :dst} last-captured]
  (let [color (get-color piece)
        [i j] src
        similar-pieces (->> (get-in game [:pieces color])
                            (filter (fn [[p pos]] (and (not= pos src)
                                                       (= piece p))))
                            (map second)
                            (filter #(let [arg %
                                           moves (get-in game [:valid-moves arg])]
                                       (contains? moves dst))))
        disambiguate-rank (some #(not= (first %) i) similar-pieces)
        disambiguate-file (or
                           (and (or (= :wp piece) (= :bp piece)) last-captured)
                           (some #(not= (second %) j) similar-pieces))]
    (str (or
          (when disambiguate-file (j-to-file j))
          (when disambiguate-rank (i-to-rank i))))))

(defn translate-move-to-pgn [{:keys [last-event move game
                                     check check-mate last-captured]}]
  (let [check-str (cond
                    check-mate "#"
                    check "+"
                    :else "")]

    (case last-event
      :right-castling (str "O-O" check-str)
      :left-castling (str "O-O-O" check-str)
      (let [piece (get-p game (:src move))
            piece-str (piece-to-letter piece)
            capture-str (when last-captured "x")
            [dst-i dst-j] (:dst move)
            dst-str (str (get j-to-file dst-j) (get i-to-rank dst-i))
            disambiguation-str (disambiguate-move game piece move last-captured)
            promotion-str (when-let [promotion (:promotion move)] (str "=" (piece-to-letter promotion)))]
        (str piece-str disambiguation-str capture-str dst-str promotion-str check-str)))))


