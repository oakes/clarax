(ns pixi-midi-gogo.core
  (:require [clara.rules :as rules]))

(defrecord Person [id name email])

(defn insert
  ([record]
   (rules/insert! record))
  ([session record]
   (rules/insert session record)))

