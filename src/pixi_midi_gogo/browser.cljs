(ns pixi-midi-gogo.browser
  (:require [clara.rules :refer [defrule]]
            [rum.core :as rum]))

(defrecord Element [parent value])

(rum/defc empty-comp
  [content]
  content)

(defrule elem
  [Element (= ?parent parent) (= ?value value)]
  =>
  (rum/mount
    (empty-comp ?value)
    (.querySelector js/document ?parent)))

