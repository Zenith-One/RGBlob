(ns rgblob.game
    (:require [clojure.string :as string]
              [rgblob.sprite :as rs]
              [rgblob.player :as player]
              [rgblob.util :as util :refer [log-obj log]]))

(defonce ^:private state (atom {:render-list []}))

(defn get-player []
    (:player @state))

(def board [[:white :white :blue :white :white :white :green :green]
            [:white :green :green :green :red :white :black :white]
            [:green :black :red :red :red  :black :red :blue]
            [:white :blue :white   :white  :green :green :red   :blue]
            [:red :red   :white  :blue :green :green   :blue  :blue]
            [:red   :blue  :green :blue :red   :white  :green :white]
            [:red  :red :red :green   :red  :red :white   :blue]
            [:blue :white :blue   :blue  :blue :blue :blue   :red]])

(def fruit (map (fn [[x y color]]
    (let [sprite (rs/sprite {:src "/img/fruit.png"
                             :x (* x 64) :y (* y 64)
                              :width 384
                              :height 128
                              :frame-width 32
                              :frame-height 32
                              :scale-x 2
                              :scale-y 2})]
      (rs/add-anim! sprite :red [0 11])
      (rs/add-anim! sprite :green [12 23])
      (rs/add-anim! sprite :blue [24 35])
      (rs/add-anim! sprite :white [36 47])
      (rs/play! sprite color true)
      sprite)
                    
                    ) [ [1 0 :blue] 
                       [5 1 :red] 
                       [3 3 :green] 
                       [2 4 :blue]
                       [5 5 :red]
                       [7 5 :blue]
                       [6 6 :green]
                       [0 3 :green]
                       [1 7 :red]
                       [7 1 :white]
                       ]))

(def tile-frames {:white {:diff 0 :same [0]}
                  :black {:diff 1 :same [1]}
                  :red   {:diff 5 :same [6 9] }
                  :green {:diff 10 :same [11 14]}
                  :blue  {:diff 15 :same [16 19]}})

(def tile-anim [[:white-diff [0 0]] [:white-same [0 0]]
                [:black-diff [1 1]] [:black-same [1 1]]
                [:red-diff [5 5]] [:red-same [6 9]]
                [:green-diff [10 10]] [:green-same [11 14]]
                [:blue-diff [15 15]] [:blue-same [16 19]]])

(defn setup-tile-color-anims [sprite]
    (doall (map (fn [[kw range]]
        (log kw range)
        (rs/add-anim! sprite kw range)) tile-anim)))

(defn get-color-at-board-pos [x y]
    (nth (nth board y) x))

(defn color->board-anim [color t]
  (keyword (str (name color) "-" (name t))))

(defn build-board-sprite-row [row-number]
  (map  (fn [x] (let [sprite (rs/sprite { 
                                   :src "/img/tiles.png"
                                   :x (* x 64) :y (* row-number 64)
                                   :width 160
                                   :height 128
                                   :frame-width 32
                                   :frame-height 32
                                   :scale-x 2
                                   :scale-y 2})]
                 (setup-tile-color-anims sprite)
                 (let [board-anim (color->board-anim 
                                    (get-color-at-board-pos x row-number) 
                                    :diff)] 
                   (log board-anim)
                   (rs/play! sprite board-anim))
                 sprite))
           (range 8)))

(defonce board-sprites
    (map build-board-sprite-row (range 8)))

(defn input 
    "Puts a user's intended input into the state for calculation"
    [intent]
    (player/input! intent))

(defn step-all! [sprites]
    ; (log "stepping all of them" sprites)
  (dorun (map #(rs/step! %) sprites)))

(defn get-context []
    (let [id (:canvas-id @state)
          canvas (.getElementById js/document id)]
      (.getContext canvas "2d")))

(defn update-tile-frame [sprite color pcolor]
    (let [t (if (= color pcolor) :same :diff)]
        (swap! sprite assoc :frame (get-in tile-frames [color t]))))

(defn get-flat-board-sprites []
    (apply concat board-sprites))

(defn get-coords-from-sprite [sprite]
    (let [{x :x y :y 
           fw :frame-width fh :frame-height 
           sx :scale-x sy :scale-y} @sprite
          rw (* fw sx)
          rh (* fh sy)
          cx (if (> x 0) (Math/floor (/ x rw)) 0)
          cy (if (> y 0) (Math/floor (/ y rh)) 0)]
         {:x cx :y cy}))

(defn render-board-row [row]
    ; (log "row" row)
    (let [player-color (player/get-color)]
      (reduce 
          (fn [_ sprite]
            (rs/clear (get-context) sprite)
            (rs/render (get-context) sprite )) 
          true row)))

(defn render-board []
    (reduce (fn [_ row] #_(log-obj row) (render-board-row row)) true board-sprites))

(defn render-all! [sprites]
  (dorun (map #(rs/render (get-context) %) sprites)))

(defn board-anim->color [anim]
    (keyword (first (string/split (name anim) "-"))))

(defn handle-player-collision [f]
    (let [color (:active-anim @f)
          flat-sprites (apply concat board-sprites)
          same-tiles (filter #(or (= (:active-anim (deref %)) (color->board-anim color :diff))
                                   (= (:active-anim (deref %)) (color->board-anim color :same)))
                              flat-sprites)
          diff-tiles (filter #(and (not (= (:active-anim (deref %)) 
                                            (color->board-anim color :diff)))
                                    (not (= (:active-anim (deref %)) 
                                            (color->board-anim color :same))))
                              flat-sprites)]
        (player/set-color! color)
        (doall 
            (map #(rs/play! % (color->board-anim color :same)) 
                  same-tiles))
        (doall 
            (map #(rs/play! % (color->board-anim 
                                (board-anim->color 
                                    (:active-anim (deref %))) 
                                        :diff))
                  diff-tiles))))

(defn check-player-collision []
  (let [p (get-player)] 
    (reduce (fn [_ f] 
                (if (and (= (:x @f) (:x @p)) (= (:y @f) (:y @p)))
                  (do 
                    (handle-player-collision f)
                    (swap! state assoc :fruit (filter #(not (= % f)) (:fruit @state)))
                    (swap! state assoc :render-list (filter #(not (= % f)) (:render-list @state))))))
            true
            (:fruit @state))))

(defn update-fn []
    (let [sprites (:render-list @state)
          in (:input @state)]
      (step-all! sprites)
      (step-all! (get-flat-board-sprites))
      (player/update-player board)
      (check-player-collision)
      (render-board)
      (render-all! sprites)))

(swap! state assoc :update update-fn)

(defn setup! [canvas-id]
    (swap! state assoc :canvas-id canvas-id)
    (player/set-canvas-id! canvas-id))

(defn add-sprite [sprite]
    (swap! state assoc :render-list (conj (:render-list @state) sprite)))

(defn do-update []
    (let [ufunc (:update @state)]
        (ufunc)))

(defn add-fruit []
  (let [sprites (:render-list @state)
        new-sprites (reduce (fn [c n] (conj c n)) sprites fruit)] 
    (swap! state assoc :fruit fruit)
    (swap! state assoc :render-list new-sprites)))

(defn start! []
    (if (nil? (:canvas-id @state))
      false 
      (swap! state assoc :interval (js/setInterval do-update 100))))

(defn stop! []
    (js/clearInterval (:interval @state)))

(defn log-state []
    (log-obj @state))

(defn set-player! [sprite]
    (swap! state assoc :player sprite)
    (player/set-sprite! sprite))
