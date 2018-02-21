(ns pixel-midi-gogo.event
  (:require [pixel-midi-gogo.core :refer [insert *session ->Fact]]
            [clara.rules :as rules]))

(defrecord Event [id type options])

(defn jsx->clj
  [x]
  (into {} (for [k (.keys js/Object x)] [k (aget x k)])))

(defn add-event [id e]
  (let [{:strs [type] :as opts} (jsx->clj e)]
    (swap! *session
      (fn [session]
        (-> session
            (insert (->Fact (keyword type (name id)) (->Event id type opts)))
            rules/fire-rules)))))

