(ns pixi-midi-gogo.core
  (:require [clara.rules :as rules]))

(defrecord Fact [id timestamp value])

(defn insert
  ([id value]
   (rules/insert! (->Fact id (.getTime (js/Date.)) value))
   (when (record? value)
     (rules/insert! value)))
  ([session id value]
   (cond-> session
           true (rules/insert (->Fact id (.getTime (js/Date.)) value))
           (record? value) (rules/insert value))))

