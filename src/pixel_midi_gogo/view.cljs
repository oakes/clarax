(ns pixel-midi-gogo.view
  (:require [clara.rules :refer [defrule]]
            [rum.core :as rum]
            [clojure.walk :as walk]
            [pixel-midi-gogo.core :refer [delete]]
            [pixel-midi-gogo.event :refer [add-event]]
            [clara.rules.accumulators :refer [all]]))

(defrecord View [parent value])

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
  [?views <- (all) :from [View (= ?parent parent)]]
  =>
  (when-let [{:keys [parent value]} (delete ?views)]
    (-> (walk/postwalk update-attrs value)
        empty-comp
        (rum/mount (.querySelector js/document parent)))))

