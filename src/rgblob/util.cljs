(ns rgblob.util)

(def log (.-log js/console))

(defn log-obj [obj] 
    (log (clj->js obj)))