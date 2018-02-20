(ns pixi-midi-gogo.browser
  (:require [clara.rules :as rules :refer [defrule]]
            [rum.core :as rum]
            [clojure.walk :as walk]
            [pixi-midi-gogo.core :refer [insert *session]]))

(defrecord Element [parent value])

(defrecord Event [id type])

(rum/defc empty-comp
  [content]
  content)

(defrule elem
  [Element (= ?parent parent) (= ?value value)]
  =>
  (-> (walk/postwalk
        (fn [x]
          (if (and (map? x) (:id x))
            (assoc x :on-click #(swap! *session
                                  (fn [session]
                                    (-> session
                                        (insert ::clicks (->Event (:id x) "click"))
                                        rules/fire-rules))))
            x))
        ?value)
      empty-comp
      (rum/mount (.querySelector js/document ?parent))))

