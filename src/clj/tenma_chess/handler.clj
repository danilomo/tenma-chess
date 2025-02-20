(ns tenma-chess.handler
  (:require
   [integrant.core :as ig]
   [reitit.ring :as reitit-ring]
   [tenma-chess.middleware :refer [middleware]]
   [hiccup.page :refer [include-js include-css html5]]
   [config.core :refer [env]]
   [tenma-chess.websocket :refer [chess-handler]]
   [ring.middleware.params :as params]))

(def mount-target
  [:div#app
   [:h2 "Welcome to tenma-chess"]
   [:p "please wait while Figwheel/shadow-cljs is waking up ..."]
   [:p "(Check the js console for hints if nothing exciting happens.)"]])

(defn head []
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1"}]
   (include-css (if (env :dev) "/css/site.css" "/css/site.min.css"))])

(defn loading-page []
  (html5
   (head)
   [:body {:class "body-container"}
    mount-target
    (include-js "/js/app.js")]))

(defn index-handler
  [_request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (loading-page)})

(def app
  (params/wrap-params (reitit-ring/ring-handler
                       (reitit-ring/router
                        [["/" {:get {:handler index-handler}}]
                         ["/chess" chess-handler]])
                       (reitit-ring/routes
                        (reitit-ring/create-resource-handler {:path "/" :root "/public"})
                        (reitit-ring/create-default-handler))
                       {:middleware middleware})))

(defmethod ig/init-key :app/handler [_ {:keys [chess-server]}]
  (println "Iniciou handler")
  (params/wrap-params (reitit-ring/ring-handler
                       (reitit-ring/router
                        [["/" {:get {:handler index-handler}}]
                         ["/chess" (chess-handler chess-server)]])
                       (reitit-ring/routes
                        (reitit-ring/create-resource-handler {:path "/" :root "/public"})
                        (reitit-ring/create-default-handler))
                       {:middleware middleware})))

