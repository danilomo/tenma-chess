(ns tenma-chess.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :as a :refer [<! >!]]
   [tenma-chess.chess :as chess :refer [get-p piece-map get-color make-move find-moves new-game]]
   [tenma-chess.algebraic :as algebraic :refer [parse-pgn make-move-algebraic]]
   [reagent.core :as r]
   [clojure.edn :as edn]
   [clojure.string :as string]
   [reagent.dom :as rdom]
   [haslett.client :as ws]
   [haslett.format :as fmt]))

(def initial-game (new-game))
(def game (r/atom initial-game))
(def match-info (r/atom {:status :not-started}))
(def previous-games (r/atom []))
(def state (r/atom {}))
(def available (r/atom #{}))
(def selected (r/atom nil))
(def text (r/atom "{}"))
(def canvas-size 400)
(def square-size (quot canvas-size 8))

(declare repaint!)

; websocket functions
(defn wait-for-move! []
  (go (let [ws (:ws @match-info)
            in (:source ws)
            msg (<! in)
            move-from-adv (clojure.edn/read-string (:move msg))]
        (swap! game #(make-move % move-from-adv))
        (repaint!)
        (swap! match-info assoc :my-turn true))))

(defn connect-ws! []
  (go (let [stream (<! (ws/connect "ws://localhost:8080/chess" {:format fmt/edn}))
            info (<! (:source stream))
            my-turn (= :white (:color info))]
        (reset! match-info (merge {:ws stream :state :started :my-turn my-turn} info))
        (when-not (:my-turn @match-info) (wait-for-move!)))))

(defn send-move! [move]
  (let [ws (:ws @match-info)
        out (:sink ws)]
    (go
      (>! out move)
      (swap! match-info assoc :my-turn false))
    (wait-for-move!)))

; painting functions

(defn color-square [i j]
  (let
   [white (if (even? i) (even? j) (odd? j))]
    (cond
      (and @selected (= @selected [i j])) "blue"
      (and @available (@available [i j])) "green"
      white "white"
      :else "#767676")))

(defn draw-pieces [ctx game square]
  (doseq [[i j] (for [i (range 0 8) j (range 0 8)] [i j])]
    ;; draw coordinates (for debugging purposes)
    (set! (.-font ctx) "12px Arial")
    (set! (.-fillStyle ctx) "black")
    (.fillText ctx (str " "
                        (char (+ algebraic/A j))
                        (char (- algebraic/EIGHT i))
                        " " i "-" j)
               (* j square)
               (* (inc i)
                  square))
    ;; draw piece icon on board
    (set! (.-font ctx) (str (int (* square 0.8)) "px Arial"))
    (set! (.-fillStyle ctx) "black")
    (let [piece (get-p game i j)
          offset (int (* square 0.16))]
      (when piece
        (.fillText ctx
                   (piece piece-map)
                   (+ offset (* j square))
                   (- (* (inc i) square) offset))))))

(defn draw-chessboard [canvas game]
  (let [ctx (.getContext canvas "2d")
        square (/ (.-width canvas) 8)]
    ;; draw the board
    (.clearRect ctx 0 0 canvas-size canvas-size)
    (doseq [[i j] (for [i (range 0 8) j (range 0 8)] [i j])]
      (set! (.-fillStyle ctx) (color-square i j))
      (.fillRect ctx (* j square) (* i square) square square))
    ;; draw the pieces
    (draw-pieces ctx game square)
    ;; draw the board frame
    (set! (.-strokeStyle ctx) "black")
    (set! (.-lineWidth ctx) 2)
    (.strokeRect ctx 1 1 (- (.-width canvas) 2) (- (.-height canvas) 2))))

(defn coordinate-to-index
  "convert board coordinate value (x or y) to matrix index (i or j)"
  [x]
  (->> (take-while #(> x %) (iterate #(+ square-size %) 0))
       count
       dec))

(defn chess-click-handler [e state]
  (let [turn (:turn @game)
        xEvt (.-clientX e)
        yEvt (.-clientY e)
        x (-  xEvt (-> e .-target .-offsetLeft))
        y (- yEvt (-> e .-target .-offsetTop))
        j (coordinate-to-index x)
        i (coordinate-to-index y)
        move {:src @selected :dst [i j] :promotion (if (even? turn) :wq :bq)}]
    (cond (= @selected [i j])
          (do
            (reset! selected nil)
            (reset! available nil))
          (and @selected (@available [i j]))
          (do
            (swap! previous-games conj @game)
            (swap! game #(make-move % move))
            (send-move! move)
            (reset! selected nil)
            (reset! available nil))
          :else
          (do
            (reset! selected [i j])
            (reset! available (find-moves @game [i j]))))
    (draw-chessboard (:canvas @state) @game)))

(defn repaint! []
  (draw-chessboard (:canvas @state) @game))

(defn chess [{size :size}]
  (r/create-class {:component-did-mount (fn []
                                          (draw-chessboard (:canvas @state) @game))
                   :reagent-render (fn [] [:canvas {:id "my-canvas"
                                                    :on-click #(when (:my-turn @match-info)
                                                                 (chess-click-handler % state))
                                                    :width size
                                                    :height size
                                                    :ref #(swap! state assoc :canvas %)}])}))

(defn get-captured [color]
  (->> (:captured-pieces @game)
       (filter #(= color (get-color %)))
       (map piece-map)))

(defn white-plays []
  (even? (or (:turn @game) 1)))

(defn captured [pieces]
  [:span (string/join "  " pieces)])

(defn game-status []
  [:div
   [:h3 (str "Player turn: " (if (white-plays) "white" "black"))]
   (and (not-empty (:captured-pieces @game))
        [:span
         [:h5 "Captured pieces"]
         [captured (get-captured :white)]
         [:br]
         [captured (get-captured :black)]])])
(defn app []
  [:div
   [:p (str (dissoc @match-info :ws))]
   [:button {:on-click (fn [] (println @game))} "Log board"]
   [:button {:on-click (fn []
                         (when (not-empty @previous-games)
                           (reset! game (last @previous-games))
                           (swap! previous-games pop)
                           (repaint!)))} "Undo"]
   [:br]
   (or (when (:check-mate @game) [:h1 "CHECK MATE!"])
       (when (:check @game) [:h1 "Check!"]))
   [chess {:size canvas-size}]
   [:br]
   [:button {:on-click #(do (reset! game (edn/read-string @text))
                            (repaint!))} "Load board"]
   [:button {:on-click #(let [new-g (->> @text
                                         (parse-pgn)
                                         (:moves)
                                         (reduce (fn [g move] (make-move-algebraic g move)) (new-game)))]
                          (reset! game new-g)
                          (repaint!))} "Play game"]
   [:br]
   [:textarea {:rows 4 :on-change #(reset! text (-> % .-target .-value))}]
   [:br]
   [game-status]])

(defn mount-root []
  (connect-ws!)
  (rdom/render [app] (.getElementById js/document "app")))

(defn init! []
  (mount-root))

(defn ^:dev/after-load reload! []
  (mount-root))
