(ns tenma-chess.tcp
  (:require [aleph.tcp :as tcp]
            [gloss.core :as gloss]
            [gloss.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [manifold.deferred :as d]
            [manifold.stream :as s]))

(def protocol (gloss/string :utf-8 :delimiters ["\n" "\r\n"]))

(defn echo-handler [s info]
  (let [out (s/map str/upper-case (io/decode-stream s protocol))]
    (s/connect
     (s/map #(io/encode protocol %) out)
     s)))
    

(defn start-server [] (tcp/start-server echo-handler {:port 10001}))







