(ns pixi-midi-gogo.browser
  (:require [clara.rules :refer [defrule]]
            [rum.core :as rum]))

(defrecord Element [value parent])

(rum/defc empty-comp
  [content]
  content)

(defrule elem
  [?elem <- Element]
  =>
  (rum/mount
    (empty-comp (:value ?elem))
    (.querySelector js/document (:parent ?elem))))

