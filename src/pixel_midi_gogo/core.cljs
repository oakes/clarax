(ns pixel-midi-gogo.core
  (:require [clara.rules :as rules]))

(def *session (atom nil))

(defrecord Def [id value])

(defn insert
  ([fact]
   (let [meta-map {:timestamp (.getTime (js/Date.))}]
     (rules/insert! (with-meta fact meta-map))
     (when (and (instance? Def fact)
                (record? (:value fact)))
       (rules/insert! (with-meta (:value fact) meta-map)))))
  ([session fact]
   (let [meta-map {:timestamp (.getTime (js/Date.))}]
     (cond-> session
             true
             (rules/insert (with-meta fact meta-map))
             (and (instance? Def fact)
                  (record? (:value fact)))
             (rules/insert (with-meta (:value fact) meta-map))))))

(defn delete
  [facts]
  (let [facts (sort-by #(-> % meta :timestamp) facts)
        current-fact (last facts)
        old-facts (butlast facts)]
    (doseq [{:keys [value] :as fact} old-facts]
      (rules/retract! fact)
      (when (and (record? value)
                 (not= value (:value current-fact)))
        (rules/retract! value)))))

