(ns pixel-midi-gogo.event
  (:require [pixel-midi-gogo.core :as pmg-core]))

(defrecord Event [data options timestamp])

(defmethod pmg-core/action
  "event-insert"
  [_ event]
  (swap! pmg-core/*session pmg-core/insert event))

(defmethod pmg-core/action
  "event-edit"
  [_ {:keys [old new]}]
  (swap! pmg-core/*session pmg-core/edit old new))

