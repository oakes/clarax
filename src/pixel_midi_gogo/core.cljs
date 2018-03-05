(ns pixel-midi-gogo.core
  (:require [clara.rules :as rules])
  (:import goog.net.XhrIo))

(def *session (atom nil))

(defrecord Def [id value timestamp])

(defn insert
  ([fact]
   (rules/insert! fact)
   (when (and (instance? Def fact)
              (record? (:value fact)))
     (rules/insert! (:value fact))))
  ([session fact]
   (cond-> session
           true
           (rules/insert fact)
           (and (instance? Def fact)
                (record? (:value fact)))
           (rules/insert (:value fact)))))

(defn delete [fact]
  (rules/retract! fact)
  (rules/retract! fact))

(defn edit [fact new-args]
  (rules/retract! fact)
  (rules/retract! fact)
  (rules/insert! (merge fact new-args)))

(defn delete-defs
  [facts]
  (let [facts (sort-by #(-> % meta :timestamp) facts)
        current-fact (last facts)
        old-facts (butlast facts)]
    (doseq [{:keys [value] :as fact} old-facts]
      (rules/retract! fact)
      (when (and (record? value)
                 (not= value (:value current-fact)))
        (rules/retract! value)))
    current-fact))

(defn watch-files [files]
  (when-not js/COMPILED
    (.send XhrIo
      "/watch"
      (fn [])
      "POST"
      (pr-str files))))

