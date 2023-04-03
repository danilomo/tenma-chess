(ns tenma-chess.server
  (:require
   [tenma-chess.handler :refer [app]]
   [tenma-chess.tcp-server :refer [start-server!]]
   [config.core :refer [env]]
   [aleph.http :as http])
  (:gen-class))

(defn -main [& _]
  (let [port (or (env :port) 3000)]
    (http/start-server #'app {:port port})))

(defn -main___ [& _]
  (let [port (or (env :port) 3000)]
    (start-server! port)))

