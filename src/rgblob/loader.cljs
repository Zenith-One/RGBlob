(ns rgblob.loader)

(def ^:private images (atom {}))

(defn add-img!
    "Takes a name (symbol) and URL string and creates an image,
     then adds that image to the list of managed images."
    [name url]
    (let [img (js/Image.)]
        (set! (.-src img) url)
        (swap! images assoc name img)))

(defn img [name]
    (name @images))