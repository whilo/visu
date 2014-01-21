(ns visu.client
  (:require goog.net.WebSocket
            [goog.color :as gcolor]
            [dommy.core :as dom]
            [hiccups.runtime :as hiccupsrt]
            [cljs.reader :refer [read-string]]
            [clojure.browser.repl]
            [cljs.core.async :as async :refer [>! <! chan put! take! timeout close! map< map> filter< filter>]]
            [visu.artist :refer [clear-canvas draw-text draw-arc draw-line draw-rect generate-random-color rgb-to-string]]
            [visu.sculptor :as sculptor])
  (:require-macros [hiccups.core :as hiccups]
                   [cljs.core.async.macros :refer [go alt!]]
                   [dommy.macros :refer [sel sel1 node deftemplate]]))

;; fire up repl
#_(do
    (def repl-env (reset! cemerick.austin.repls/browser-repl-env
                         (cemerick.austin/repl-env)))
    (cemerick.austin.repls/cljs-repl repl-env))

(def websocket* (atom nil))

(def sketch-state ;; unify state in a single atom or synchronization becomes an issue
  (atom
   {:width 1100
    :height 700
    :drawing true}))

(def sketch-data (atom {:data nil :selected nil}))

(defn log [& args]
  (.log js/console (apply str args)))

;; maybe this was overkill, not sure, but the log fn is fine
(set! clojure.core/*print-fn* (fn [& s] (.log js/console (apply str s))))

;; --- WEBSOCKET CONNECTION ---

(defn send! [data]
  (.send @websocket* (str data)))


(defn- take-all! [raw-message]
  (let [message (read-string raw-message)
        data (message :data)]
    (do
      (log (str "data received!"))
      (swap! sketch-data assoc :data data))))


(defn client-connect! []
  (log "establishing websocket ...")
  (reset! websocket* (js/WebSocket. "ws://localhost:9090"))
  (doto @websocket* ;; use doto when operating on a single object
    (aset "onopen" #(do (log "channel opened")
                        (.send @websocket* {:type "greeting" :data []})))
    (aset "onclose" #(log "channel closed"))
    (aset "onerror" #(log (str "ERROR: " %)))
    (aset "onmessage" #(let [data (.-data %)]
                         (take-all! data))))
  (set! (.-onclick (sel1 :#disconnect-button)) (fn [] (.close @websocket*) (reset! websocket* nil)))
  (log "websocket loaded."))


;; --- DRAWING STUFF ---

(def palette [[2 56 88] ;; keep data transparent for manipulation
              [4 90 141]
              [5 112 176]
              [54 144 192]
              [116 169 207]
              [166 189 219]
              [208 209 230]
              [236 231 242]
              [255 247 251]])

(defn color [temperature] ;; and only generate runtime representations when necessary
  ((mapv rgb-to-string palette) temperature))

(defn math-log [x base]
  (/ (Math/log x) (Math/log base)))


(defn cleanup []
  (clear-canvas (deref sketch-state) (sel1 :#the-canvas))
  (swap! sketch-state assoc :drawing false))


(defn draw-cancer-graph [] ;; split, possibly one or more functions generating the data
  (let [raw-data (@sketch-data :data)
        canvas (sel1 :#the-canvas)
        data (vals raw-data)
        cancer-type (vec (keys raw-data))
        scale (/ 400.0 35000)
        y-start 450
        bar-width 600
        step (/ 600 (count data))
        the-children (mapv #(% "Children") data)
        mid-adults (mapv #(% "Mid Adults") data)
        older-adults (mapv #(% "Older Adults") data)
        summarized (mapv #(reduce + (vals %)) data)
        sorted-summarized (->> (map vector (range (count data)) summarized)
                              (into {})
                              (sort-by val)
                              keys
                              reverse)
        sorted-hashmap (into {} (map vector (range (count data)) sorted-summarized))]
    (do ;; do in let is redundant
      (cleanup)
      (draw-text canvas (/ bar-width 2) 20 "Female Cancer Distribution" 16 "center" "#50afde")
      (doall
       (map
        #(let [child-size (- (* scale (the-children (val %))))
               mid-size (- (* scale (mid-adults (val %))))
               older-size (- (* scale (older-adults (val %))))
               x (+ 5 (* (key %) step))]
           (do
             (draw-rect
              canvas
              x y-start
              (- step 5) child-size
              "#BB66AA")
             (draw-rect
              canvas
              x (+ y-start child-size)
              (- step 5) mid-size
              "#AABB66")
             (draw-rect
              canvas
              x (+ y-start child-size mid-size)
              (- step 5) older-size
              "#66AABB")
             (draw-text canvas (+ x (/ step 2)) (+ y-start 20) (cancer-type (val %)) 14 "center" "#7070aa")))
        sorted-hashmap)))))


(defn draw-word [word x-position y-position words-list]
  (let [canvas (sel1 :#the-canvas)
        ctx (.getContext canvas "2d")
        meta-data (@sketch-state :meta-data)
        a (Math/sqrt (/ (- (val word) (meta-data :n-min))
                        (- (meta-data :n-max) (meta-data :n-min))))
        text-size (+
                   (* (- 1 a) (meta-data :text-min))
                   (* a (meta-data :text-max)))]
    (set! (.-font ctx) (str text-size "px Open Sans"))
    (set! (.-textAlign ctx) "left")
    (set! (.-fillStyle ctx) "#66AABB")
    (let [x-start (if (< 1100 (+ x-position (.-width (.measureText ctx (key word)))))
                    0
                    x-position)
          y-start (if (< 1100 (+ x-position (.-width (.measureText ctx (key word)))))
                    (+ y-position (meta-data :text-max))
                    y-position)]
      (.fillText ctx (key word) x-start y-start)
      (if-not (seq words-list)
        (log "done")
        (draw-word (first words-list) (+ x-start (.-width (.measureText ctx (key word)))) y-start (rest words-list))))))


(defn draw-word-cloud []
  (let [data (remove #(< (val %) 5) (@sketch-data :data))
        canvas (sel1 :#the-canvas)
        ctx (.getContext canvas "2d")]
    (swap! sketch-state assoc :meta-data
           {:n-max (apply max (vals data))
            :n-min 5
            :text-min 2
            :text-max 50})
    (cleanup)
    (set! (.-fillStyle ctx) "#202035")
    (.fillRect ctx 0 0 (@sketch-state :width) (@sketch-state :height))
    (draw-word (first data) 0 50 (rest data))))


(defn draw-spiral [theta r]
  (let [canvas (sel1 :#the-canvas)
        x (+ (/ (@sketch-state :width) 2) (* r (Math/cos theta)))
        y (+ (/ (@sketch-state :height) 2)(* r (Math/sin theta)))]
    (if (@sketch-state :drawing)
      (do
        (draw-arc canvas x y 1 0 (* 2 Math/PI) (rgb-to-string (generate-random-color [255 255 255])))
        (if (>= theta (* 256 Math/PI))
          (log "done")
          (go
            (<! (timeout 1))
            (draw-spiral (+ theta (* 3 (/ Math/PI 200))) (+ r 0.05)))))
      (println "Drawing interrupted ..."))))


(defn add-vectors [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn delta-vectors [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn vector-length [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn scalar-mult-vector [k [x y]]
  [(* k x) (* k y)])

(defn prepare-graph-data [data] ;; can be split further?
  (let [edges (@sketch-data :data)
        vertices (->> @sketch-state
                      :data
                      (map #(into [] %))
                      flatten
                      (into #{})
                      (map #(vector % {:position [(+ (/ (@sketch-state :width) 2) (* (rand 550) (Math/cos (rand (* 64 Math/PI)))))
                                                  (+ (/ (@sketch-state :height) 2) (* (rand 350) (Math/sin (rand (* 64 Math/PI)))))]
                                       :displacement [0 0]}))
                      (into {}))]
    (swap! sketch-data assoc :data {:edges edges :vertices vertices :constants {:k (* 0.25 (Math/sqrt (/ (* (@sketch-state :width) (@sketch-state :height)) (count vertices))))}})))


(defn draw-force-based-graph []
  (let [data (-> @sketch-data :data)
        sketch-height (@sketch-state :height)
        sketch-width (@sketch-state :width)
        canvas (sel1 (@sketch-state :canvas))
        ctx (.getContext canvas "2d")
        vertices ()]
    (cleanup)
    (doall ;; doseq?
     (map #(draw-arc canvas (-> % val :x) (-> % val :y) 3  0 (* 2 Math/PI) "#ffffff") data))))


(defn draw-scalar-weather-data []
  (let [data (@sketch-data :data)
        cells  (mapv #(assoc % :color-index (dec (Math/round (/ (math-log (% :value) 10) -2)))) (data :cells))
        x-stepsize (/ (@sketch-state :width) (dec (data :x-dim)))
        y-stepsize (/ (@sketch-state :height) (dec (data :y-dim)))
        canvas (sel1 :#the-canvas)]
    (cleanup)
    (doseq [x (-> data :x-dim dec range) ;; double loop, like for-comprehension
            y (-> data :y-dim dec range)]
      (draw-rect canvas
                 (* x x-stepsize)
                 (* y y-stepsize)
                 x-stepsize
                 y-stepsize
                 (color ((cells (+ x (* y (dec (data :x-dim))))) :color-index))))
    #_(doall
     (map
      (fn [x]
        (do
          (doall
           (map
            #(draw-rect
              canvas
              (* x x-stepsize)
              (* % y-stepsize)
              x-stepsize
              y-stepsize
              (color ((cells (+ x (* % (dec (data :x-dim))))) :color-index)))
            (range (dec (data :y-dim)))))))
      (range (dec (data :x-dim)))))))

;; --- HTML STUFF ---

(defn enable-buttons []
  (do
    (set! (.-onclick (sel1 :#connect-button)) (fn [] (client-connect!)))
    (set!
     (.-onclick (sel1 :#cancer-bar-graph-button))
     (fn [] (go
             (send! {:type "get" :data "cancer"})
             (<! (timeout 500))
             (draw-cancer-graph)
             (dom/set-text! (sel1 :#header-title) "Cancer Graph"))))
    (set!
     (.-onclick (sel1 :#word-cloud-button))
     (fn [] (go
             (send! {:type "get" :data "wordcloud"})
             (<! (timeout 500))
             (draw-word-cloud)
             (dom/set-text! (sel1 :#header-title) "Wordcloud"))))
    (set!
     (.-onclick (sel1 :#scalar-weather-data-button))
     (fn [] (go
             (send! {:type "get" :data "weatherdata"})
             (<! (timeout 2000))
             (draw-scalar-weather-data)
             (dom/set-text! (sel1 :#header-title) "Prepicipation Worldwide"))))
    (set!
     (.-onclick (sel1 :#spiral-button))
     (fn [] (go
             (cleanup)
             (swap! sketch-state assoc :drawing true)
             (draw-spiral 0 0)
             (dom/set-text! (sel1 :#header-title) "Spiral"))))
    (set!
     (.-onclick (sel1 :#real-3d-button))
     (fn [] (go
             (cleanup)
             (sculptor/work-you-sucker! "webgl-canvas")
             (dom/set-text! (sel1 :#header-title) "3D on 2D, hot!"))))
    (set!
     (.-onclick (sel1 :#clear-canvas-button))
     (fn [] (cleanup)))))


(defn create-nav []
  (let [body (sel1 :body)]
    (dom/append!
     body
     [:nav
      [:ul
       [:li.cat1 [:a  "Home"]
        [:ul
         [:li [:a#connect-button  "Connect"]]
         [:li [:a#disconnect-button "Disconnect"]]]]
       [:li.cat2
        [:a  "Drawings"]
        [:ul
         [:li [:a#cancer-bar-graph-button  "Cancer bar graph"]]
         [:li [:a#word-cloud-button "Word Cloud"]]
         [:li [:a#scalar-weather-data-button "Draw Weather Data"]]
         [:li [:a#spiral-button "Spiral"]]
         [:li [:a#real-3d-button "No way, its 3D!!1"]]
         [:li [:a#clear-canvas-button "Clear"]]]]
       [:li.cat3 [:a#header-title "Title"]]]])))


(defn init []
  (let [body (sel1 :body)
        state (deref sketch-state)]
    (do
      (create-nav)
      (dom/append! body [:div#canvas-div [:canvas#the-canvas {:width (state :width) :height (state :height)}]
                         [:canvas#webgl-canvas {:width (state :width) :height (state :height)}]])
      (enable-buttons)
      (client-connect!))))


(set! (.-onload js/window) init)


;; live-coding stuff

#_(client-connect!)
#_(@sketch-state :drawing)
#_(send! {:type "get" :data "graph"})

#_(cleanup)
