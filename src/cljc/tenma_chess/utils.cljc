(ns tenma-chess.utils
  (:require [clojure.string :as string]
            [tenma-chess.chess :as chess :refer [piece-map]]))

(defn print-line [index row]
  (str (- 8 index) "  |"  (string/join "|" (map #(str " " (or (piece-map %) " ") " ") row)) "|"))

(def separator (str "    " (string/join " " (repeat 8 "___")) " "))

(defn print-game [{board :board}]
  (let [indexed-rows (map-indexed vector board)
        contents   (string/join "\n"
                                (map #(str separator "\n" (print-line (first %) (second %))) indexed-rows))]
    (str contents
         "\n"
         separator
         "\n     a   b   c   d   e   f   g   h  \n")))
