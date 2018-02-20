(ns pixel-midi-gogo.element
  (:require [clara.rules :refer [defrule]]
            [rum.core :as rum]
            [clojure.walk :as walk]
            [pixel-midi-gogo.event :refer [add-event]]))

(defrecord Element [parent value])

(rum/defc empty-comp
  [content]
  content)

(defrule elem
  [Element (= ?parent parent) (= ?value value)]
  =>
  (-> (walk/postwalk
        (fn [x]
          (if (and (vector? x)
                   (map? (second x)))
            (update x 1
              (fn [attrs]
                (reduce
                  (fn [new-attrs [k v]]
                    (assoc new-attrs
                      k (if (.startsWith (name k) "on-")
                          (if-let [id (:id attrs)]
                            (partial add-event id)
                            (throw (js/Error. (str k " must be accompanied by :id"))))
                          v)))
                  {}
                  attrs)))
            x))
        ?value)
      empty-comp
      (rum/mount (.querySelector js/document ?parent))))

