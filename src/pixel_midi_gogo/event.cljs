(ns pixel-midi-gogo.event
  (:require [pixel-midi-gogo.core :refer [insert *session ->Def]]
            [clara.rules :as rules]
            [clojure.walk :as walk]))

(defrecord Event [data options])

(defn jsx->clj
  [x]
  (into {} (for [k (.keys js/Object x)] [k (aget x k)])))

(defn add-event [data e]
  (let [{:keys [type]
         :as opts} (->> e
                        jsx->clj
                        walk/keywordize-keys)]
    (swap! *session
      (fn [session]
        (-> session
            (insert (->Event data opts))
            rules/fire-rules)))))

