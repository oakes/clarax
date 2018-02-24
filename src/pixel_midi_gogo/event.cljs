(ns pixel-midi-gogo.event
  (:require [pixel-midi-gogo.core :refer [insert *session ->Def]]
            [clara.rules :as rules]))

(defrecord Event [data options])

(defn jsx->clj
  [x]
  (into {} (for [k (.keys js/Object x)] [k (aget x k)])))

(defn add-event [data e]
  (swap! *session
    (fn [session]
      (-> session
          (insert (->Def (keyword "event" type) (->Event data (jsx->clj e))))
          rules/fire-rules))))

