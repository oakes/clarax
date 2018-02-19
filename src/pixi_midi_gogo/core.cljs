(ns pixi-midi-gogo.core
  (:require [clara.rules :as rules]))

(defrecord Fact [id timestamp record])

(defn insert
  ([id record]
   (rules/insert! (->Fact id (.getTime (js/Date.)) record))
   (rules/insert! record))
  ([session id record]
   (-> session
       (rules/insert (->Fact id (.getTime (js/Date.)) record))
       (rules/insert record))))

