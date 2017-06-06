(ns rgblob.core
  (:require [reagent.core :as reagent :refer [atom]]
      [goog.events :as events]
      [rgblob.game :as game]
      [rgblob.sprite :as rs]
      [rgblob.player :as player]
      [rgblob.util :as util :refer [log log-obj]])
  (:import [goog.events EventType]))

(enable-console-print!)

(def jquery (js* "$"))

(def code->input
  "Map from keycode to user intention.
  
  Borrowed from https://gist.github.com/tomconnors/8460406"
  {13 :enter
   27 :escape
   37 :left
   38 :up
   39 :right
   40 :down
   65 :left
   87 :up
   68 :right
   83 :down
   })

(defonce state 
    (atom {:last-key :none}))

(defn assoc-in-multiple [base & settings-pairs]
  (reduce (fn set-settings-pair [acc pair]
            (assoc-in acc [(first pair)] (second pair)))
          base
          settings-pairs))

(defn default-square-props [i j]
  {:width 1
   :height 1
   :fill "#AAA"
   :x i
   :y j
   :stroke "#777"})

(defn render-square 
  "Renders a square of a given type, :red, :green, :blue, or :white"
  [x-pos y-pos color]
  [:rect (assoc-in-multiple (default-square-props x-pos y-pos)
                             [:fill ])])


(defn rgblob []
    [:div { :class-name "container" }
      [:h1 "RGBlob"]
      [:canvas {:width 512
                :height 512
                :id "game"}]
      ])


(reagent/render-component [rgblob]
                          (. js/document (getElementById "app")))

(def KEY-EVENT "keydown")

(def canvas (js/document.getElementById "game"))

(defn handle-key-down [e]
  (let [input (code->input (.-keyCode e))]
    (if (not (nil? input)) (do 
      (game/input input)
      (.preventDefault e)))))

(def CANVAS (.getElementById js/document "game"))

(defn get-context []
  (.getContext CANVAS "2d"))

(defn get-blob []
  (let [blob (rs/sprite { :src "/img/slime-spritesheet-calciumtrice.png"
                              :x 0 :y 0
                              :width 320
                              :height 640
                              :frame-width 32
                              :frame-height 32
                              :scale-x 2
                              :scale-y 2})]
      (rs/add-anim! blob :green-idle [0 9])
      (rs/add-anim! blob :green-roll [10 19])
      (rs/add-anim! blob :green-jump [23 29])
      (rs/add-anim! blob :green-spit [30 39])
      (rs/add-anim! blob :blue-idle [50 59])
      (rs/add-anim! blob :blue-jump [73 79])
      (rs/add-anim! blob :blue-spit [80 89])
      (rs/add-anim! blob :red-idle [100 109])
      (rs/add-anim! blob :red-jump [123 129])
      (rs/add-anim! blob :red-spit [130 139])
      (rs/add-anim! blob :white-idle [150 159])
      (rs/add-anim! blob :white-jump [173 179])
      (rs/add-anim! blob :white-spit [180 189])
      (rs/play! blob :white-idle true)
      blob))

(defonce setup-game (do 
                      (game/setup! "game")
                      (let [blob (get-blob)]
                        (game/add-sprite blob)
                        (game/add-fruit)
                        (game/set-player! blob))
                      (game/start!)))

; bind to the keydown evnt on the document
(defn setup []
  (let [doc (jquery js/document)]
    (.on doc KEY-EVENT handle-key-down)))

; Because figwheel will run setup every script reload,
; we have to safeguard against having multiple listeners
; subscribed to our event.
(defn teardown []
  (.off (jquery js/document) KEY-EVENT))


(setup)

(defn on-js-reload []
    (teardown)
    (setup))
