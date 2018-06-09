(ns pixel-midi-gogo.event
  (:require [pixel-midi-gogo.core :as pmg-core]
            [clara.rules :as rules]
            [clojure.walk :as walk]))

(defrecord Event [data options timestamp])

(defn jsx->clj
  [x]
  (into {} (for [k (.keys js/Object x)] [k (aget x k)])))

(defn add-event [data e]
  (let [{:keys [type]
         :as opts} (->> e
                        jsx->clj
                        walk/keywordize-keys)]
    (swap! pmg-core/*session pmg-core/insert (->Event data opts (.getTime (js/Date.))))))

