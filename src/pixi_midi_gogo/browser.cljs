(ns pixi-midi-gogo.browser
  (:require [clara.rules :refer [defrule]]
            [rum.core :as rum]))

(defrecord Element [id value])

(rum/defc empty-comp
  [content]
  content)

(defrule elem
  [Element (= ?id id) (= ?value value)]
  =>
  (rum/mount
    (empty-comp ?value)
    (.querySelector js/document ?id)))

