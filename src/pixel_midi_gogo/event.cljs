(ns pixel-midi-gogo.event
  (:require [pixel-midi-gogo.core :refer [insert *session ->Def]]
            [clara.rules :as rules]))

(defrecord Event [data options])

(defn jsx->clj
  [x]
  (into {} (for [k (.keys js/Object x)] [k (aget x k)])))

(defn add-event [data e]
  (let [{:keys [type]
         :as opts} (->> e
                        jsx->clj
                        (reduce
                          (fn [opts [k v]]
                            (if (instance? js/Object v)
                              opts
                              (assoc opts (keyword k) v)))
                          {}))]
    (swap! *session
      (fn [session]
        (-> session
            (insert (->Def (keyword "pixel-midi-gogo.event" type) (->Event data opts)))
            rules/fire-rules)))))

