(ns pixel-midi-gogo.utils
  (:require [goog.object :as gobj]))

(defn obj->clj [o level]
  (cond
    (goog.isObject o)
    (reduce
      (fn [result key]
        (let [v (gobj/get o key)]
          (or (when (or (not (goog.isObject v))
                        (= key "target"))
                (some->> (obj->clj v (inc level))
                         (assoc result key)))
              result)))
      {}
      (.getKeys js/goog.object o))
    (#{"string" "number"} (goog/typeOf o))
    o))

