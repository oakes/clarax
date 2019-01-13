(ns pixel-midi-gogo.utils
  (:require [pixel-midi-gogo.core :as pmg-core]
            [pixel-midi-gogo.event :refer [map->Event]]
            [goog.object :as gobj]))

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

(defn insert-event [data e]
  (let [opts (obj->clj e 0)]
    (pmg-core/send-action!
      (map->Event {:data data :options opts})
      "insert")))

(defn update-event [old-event e]
  (let [opts (obj->clj e 0)]
    (pmg-core/send-action!
      {:old old-event :new {:options opts}}
      "update")))

