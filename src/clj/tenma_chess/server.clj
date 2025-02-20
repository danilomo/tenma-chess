(ns tenma-chess.server
  (:require
   [integrant.core :as ig]
   [tenma-chess.handler :refer [app]]
   [config.core :refer [env]]
   [aleph.http :as http])
  (:gen-class))

(defn -main [& _]
  (let [port (or (env :port) 3000)]
    (http/start-server #'app {:port port})))

(defmethod ig/init-key :http/server [_ {:keys [port handler]}]
  (println "Iniciou http server")
  (http/start-server handler {:port port}))

(defmethod ig/halt-key! :http/server [_ server]
  (println "Stopeando http server")
  (.close server))
