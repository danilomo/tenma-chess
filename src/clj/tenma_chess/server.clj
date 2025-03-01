(ns tenma-chess.server
  (:require
   [integrant.core :as ig]
   [aleph.http :as http])
  (:gen-class))

(defmethod ig/init-key :http/server [_ {:keys [port handler]}]
  (println "Iniciou http server")
  (http/start-server handler {:port port}))

(defmethod ig/halt-key! :http/server [_ server]
  (println "Stopeando http server")
  (.close server))
