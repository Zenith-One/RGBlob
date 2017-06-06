(ns rgblob.sprite
    (:require [rgblob.util :as util :refer [log log-obj]]))

(defn url->image [url]
    (let [img (js/Image.)]
        (set! (.-src img) url)
        img))

(defn sprite [options]
    (let [{:keys [src height width frame-height frame-width 
                  frame x y scale-x scale-y smoothing]} options
          image (url->image src)
          h-frames (/ width frame-width)
          v-frames (/ height frame-height)
          fr (if (nil? frame) 0 frame)
          nx (if (nil? x) 0 x)
          ny (if (nil? y) 0 y)
          sx (if (nil? scale-x) 1 scale-x)
          sy (if (nil? scale-y) 1 scale-y)]
          (atom (assoc options 
                    :image image
                    :h-frames h-frames 
                    :v-frames v-frames 
                    :frame fr
                    :x nx
                    :y ny
                    :scale-x sx
                    :scale-y sy
                    :anim {}
                    :smoothing (if (nil? smoothing) false smoothing)))))

(defn add-anim! 
    "Given a sprite, an animation name, and a range of frames, add an animation to the sprite."
    [sprite name range]
    (swap! sprite update-in [:anim] assoc name range))

(defn play!
    "Given a sprite and an animation, play that animation."
    ([sprite anim] (play! sprite anim false))
    ([sprite anim looping] (play! sprite anim looping nil false))
    ([sprite anim looping next next-looping]
        (if (anim (:anim @sprite))
            (do
              (swap! sprite assoc :active-anim anim :frame (first (anim (:anim @sprite))) :loop looping)
            true)
            false)
        (if (and (not (nil? next)) (next (:anim @sprite)))
            (swap! sprite assoc :next-anim next :next-anim-looping next-looping))))

(defn step!
    "Steps the current animation of the given sprite"
    [sprite]
    (if (:active-anim @sprite)
        (let [[start end] ((:active-anim @sprite) (:anim @sprite))
              frame (:frame @sprite)
              onemore (+ 1 frame)
              not-last (>= end onemore)
              next (if not-last
                        onemore
                        (do (if (:loop @sprite) start frame)))]
             (if (and (>= onemore end) (not (:loop @sprite)) (:next-anim @sprite))
               (play! sprite (:next-anim @sprite) (:next-anim-looping @sprite) nil nil)
               (swap! sprite assoc :frame next))
             next)))

(defn get-frame-offset [sprite n]
    "Given a sprite, get frame n offset"
    (let [n0 (- n 1)
          x-pos (mod n (:h-frames sprite))
          y-pos (Math/floor (/ n (:h-frames sprite)))
          x-offset (* x-pos (:frame-width sprite))
          y-offset (* y-pos (:frame-height sprite))]
        {:x x-offset :y y-offset}))

(defn clear 
    ([context sprite] 
        (let [{x :x y :y 
               w :frame-width h :frame-height
               sx :scale-x sy :scale-y} @sprite
               rw (* w sx)
               rh (* h sy)]
               (clear context x y rw rh)))
    ([context x y width height] (.clearRect context x y width height)))

(defn render [context sprite]
    (let [dsprite (deref sprite)
        {:keys [image height width frame-height frame-width h-frames v-frames frame x y scale-x scale-y smoothing]} dsprite
        {x-offset :x y-offset :y} (get-frame-offset dsprite frame)
        rw (* scale-x frame-width)
        rh (* scale-y frame-height)]
      (set! (.-imageSmoothingEnabled context) smoothing)
      (.drawImage context image x-offset y-offset frame-width frame-height x y rw rh)
      (set! (.-imageSmoothingEnabled context) false)))
