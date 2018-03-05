(ns pixel-midi-gogo.core
  (:require [clara.rules :as rules])
  (:import goog.net.XhrIo))

(def *session (atom nil))

(defrecord Def [id value timestamp])

(defrecord Delete [record timestamp])

(defrecord Update [record timestamp])

(defn insert
  ([fact]
   (rules/insert! fact)
   (cond
     (and (instance? Def fact)
          (record? (:value fact)))
     (rules/insert! (:value fact))
     
     (instance? Delete fact)
     (do
       (rules/retract! (:record fact))
       (rules/retract! (:record fact)))
     
     (instance? Update fact)
     (do
       (rules/retract! (:record fact))
       (rules/retract! (:record fact))
       (rules/insert! (merge (:record fact)
                        (dissoc fact :record :timestamp))))))
  ([session fact]
   (when (instance? Delete fact)
     (throw (js/Error. "Can't delete here.")))
   (cond-> session
           true
           (rules/insert fact)
           (and (instance? Def fact)
                (record? (:value fact)))
           (rules/insert (:value fact)))))

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

