(ns pixel-midi-gogo.core
  (:require [clara.rules :as rules]))

(def *session (atom nil))

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

(defn delete
  [facts]
  (let [facts (sort-by :timestamp facts)
        current-fact (last facts)
        old-facts (butlast facts)]
    (doseq [{:keys [value] :as fact} old-facts]
      (rules/retract! fact)
      (when (and (record? value)
                 (not= value (:value current-fact)))
        (rules/retract! value)))))

