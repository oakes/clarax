(ns pixel-midi-gogo.view
  (:require [clara.rules :refer [defrule]]
            [rum.core :as rum]
            [clojure.walk :as walk]
            [pixel-midi-gogo.event :refer [add-event]]
            [clara.rules.accumulators :as acc]))

(defrecord View [parent value timestamp])

(rum/defc empty-comp
  [content]
  content)

(defn update-attrs [x]
  (if (and (vector? x)
           (map? (second x)))
    (update x 1
      (fn [attrs]
        (reduce
          (fn [new-attrs [k v]]
            (assoc new-attrs
              k (if (.startsWith (name k) "on-")
                  (partial add-event v)
                  v)))
          {}
          attrs)))
    x))

(defrule views
  [?view <- (acc/max :timestamp :returns-fact true)
   :from [View (= ?parent parent)]]
  =>
  (-> (walk/prewalk update-attrs (:value ?view))
      empty-comp
      (rum/mount (.querySelector js/document ?parent))))

