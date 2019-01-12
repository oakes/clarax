(ns pixel-midi-gogo.utils
  (:require [pixel-midi-gogo.core :as pmg-core]
            [pixel-midi-gogo.event :refer [->Event]]
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

(defn add-event [data e]
  (let [opts (obj->clj e 0)]
    (@pmg-core/*send-action-fn "event-insert"
     (->Event data opts (.getTime (js/Date.))))))

(defn edit-event [old-event e]
  (let [opts (obj->clj e 0)]
    (@pmg-core/*send-action-fn "event-edit"
     {:old old-event
      :new (->Event (:data old-event) opts (.getTime (js/Date.)))})))

