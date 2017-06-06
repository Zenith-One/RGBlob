(ns rgblob.player
    (:require [rgblob.sprite :as rs]
              [rgblob.util :as util :refer [log-obj log]]))

(defonce ^:private state (atom {:color :white :action :idle}))

(def color-anims {:red {:idle :red-idle 
                        :jump :red-jump
                        :spit :red-spit}
                  :blue {:idle :blue-idle 
                         :jump :blue-jump
                         :spit :blue-spit}
                  :green {:idle :green-idle 
                          :jump :green-jump
                          :spit :green-spit}
                  :white {:idle :white-idle 
                          :jump :white-jump
                          :spit :white-spit}})

(defn set-sprite! [sprite]
    (swap! state assoc :sprite sprite))


(defn set-canvas-id! [canvas-id]
    (swap! state assoc :canvas-id canvas-id))

(defn set-color! [color]
    (swap! state assoc :color color)
    (let [spit (get-in color-anims [color :spit])
          idle (get-in color-anims [color :idle])]
        (rs/play! (:sprite @state) spit false idle true)
        (swap! state assoc :action :spit)))

(defn set-anim! [action]
    (swap! state assoc :action action)
    (let [anim (get-in color-anims [(:color @state) action])]
        (rs/play! (:sprite @state) anim true)))

(defn input! [intent]
  (if (nil? (:input @state))
    (swap! state assoc :input intent)))

(defn get-canvas-dimensions []
    (if (nil? (:canvas-id @state)) 
        nil
        (let [canvas (.getElementById js/document (:canvas-id @state))
              cw (.-width canvas)
              ch (.-height canvas)]
              {:width cw :height ch})))

(def dir->offset {:left {:x -1 :y 0}
                  :right {:x 1 :y 0}
                  :up {:x 0 :y -1}
                  :down {:x 0 :y 1}})

(defn get-dsprite []
    (deref (:sprite @state)))

(defn target-valid? [target c-dim s-dim color board-color]
    (and (>= target 0) 
         (<= target (- c-dim s-dim))
         (or (= board-color :white) (= board-color color))))

(defn get-board-color-from-coords [board x y]
    (get-in board [(/ y 64) (/ x 64)]))

(defn input->target [inp board]
    (let [sprite (deref (:sprite @state))
          x (:x sprite)
          y (:y sprite)
          sx (:scale-x sprite)
          sy (:scale-y sprite)
          width (:frame-width sprite)
          height (:frame-height sprite)
          rw (* sx width)
          rh (* sy height)
          {ox :x oy :y} (inp dir->offset)
          new-x (+ x (* rw ox))
          new-y (+ y (* rh oy))
          {c-width :width c-height :height} (get-canvas-dimensions)
          board-color (get-board-color-from-coords board new-x new-y)
          x-valid (target-valid? new-x c-width rw (:color @state) board-color)
          y-valid (target-valid? new-y c-height rh (:color @state) board-color)]
          (if (and x-valid y-valid)
              {:x new-x :y new-y}
              {:x x :y y})))

(defn process-input! [board]
  (if (and (:input @state) (or (nil? (:target-x @state)) (nil? (:target-y @state))))
    (let [inp (:input @state)
          sprite (get-dsprite)
          {x :x y :y} sprite
          {tx :x ty :y} (input->target inp board)
          moving (not (and (= x tx) (= y ty)))]
      (if moving 
        (do
          (swap! state assoc :target-x tx :target-y ty))))
    (swap! state assoc :input nil)))

(defn set-velocity! []
  (if (or (nil? (:target-x @state)) (nil? (:target-y @state)))
    (do
      (swap! state assoc :target-x nil :target-y nil :input nil))
    (let [sprite (get-dsprite)
          {w :frame-width h :frame-height 
           sx :scale-x sy :scale-y
           x :x y :y} sprite
          tx (:target-x @state)
          ty (:target-y @state)
          rw (* w sx)
          rh (* h sy)
          dhv (Math.round (/ rw 4))
          dvv (Math.round (/ rh 4))
          xv (cond 
               (< tx x) (* -1 dhv)
               (> tx x) dhv
               :else 0)
          yv (cond 
               (< ty y) (* -1 dvv)
               (> ty y) dvv
               :else 0)
          ]
      (swap! state assoc :x-velocity xv :y-velocity yv))))

(defn handle-movement! []
    (let [was-moving? (:moving @state)
          sprite (:sprite @state)
          {x :x y :y} @sprite
          xv (:x-velocity @state)
          yv (:y-velocity @state)]
          (if (or (and (= xv 0) (= yv 0))
                  (and (nil? xv) (nil? yv)))
            (do
              (swap! state assoc :target-x nil :target-y nil)
              (if was-moving? 
                  (do 
                    (if (= :jump (:action @state)) 
                      (set-anim! :idle))
                    (swap! state assoc :moving false))))
            (do 
                (swap! sprite assoc :x (+ x xv) :y (+ y yv))
                (if (not was-moving?)
                  (do 
                    (set-anim! :jump)
                    (swap! state assoc :moving true)))))))

(defn update-player [board]
    (process-input! board)
    (set-velocity!)
    (handle-movement!))

(defn get-color []
    (:color @state))

(defn log-state 
  ([] (log-obj @state))
  ([k] (log-obj (k @state))) 
  ([k v] (log-obj (deref (k @state)))))