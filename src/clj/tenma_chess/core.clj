(ns tenma-chess.core
  (:require
   [integrant.core :as ig]))

(def config {:http/server {:port 8080 :handler (ig/ref :app/handler)}
             :app/handler {:chess-server (ig/ref :chess/server)}
             :chess/server {:format :pgn}})


