(ns pixel-midi-gogo.event
  (:require [pixel-midi-gogo.core :as pmg-core]))

(defrecord Event [data options timestamp])

