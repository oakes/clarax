(ns pixel-midi-gogo.event
  (:require [pixel-midi-gogo.core :as pmg-core]
            [clara.rules :as rules]
            [clojure.walk :as walk]
            #?(:cljs [goog.object :as gobj])))

(defrecord Event [data options timestamp])

(defmethod pmg-core/action
  "event-insert"
  [_ event]
  (swap! pmg-core/*session pmg-core/insert event))

#?(:cljs [(defn obj->clj [o level]
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
                (js/Object.keys o))
              (#{"string" "number"} (goog/typeOf o))
              o))
          
          (defn add-event [data e]
            (let [opts (obj->clj e 0)]
              (pmg-core/send-action "event-insert" (->Event data opts (.getTime (js/Date.))))))])

