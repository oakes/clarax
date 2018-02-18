(ns pixi-midi-gogo.core
  (:require [clara.rules :as rules]
            [rum.core :as rum]))

(defrecord Person [name email])

(defn insert
  ([record]
   (rules/insert! record))
  ([session record]
   (rules/insert session record)))

