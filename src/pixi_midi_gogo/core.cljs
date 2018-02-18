(ns pixi-midi-gogo.core
  (:require [clara.rules :as rules]))

(defn insert
  ([record]
   (rules/insert! record))
  ([session record]
   (rules/insert session record)))

