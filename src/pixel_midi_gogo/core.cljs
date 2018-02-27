(ns pixel-midi-gogo.core
  (:require [clara.rules :as rules])
  (:import goog.net.XhrIo))

(def *session (atom nil))

(defrecord Def [id value timestamp])

(defrecord Delete [record])

(defn insert
  ([fact]
   (if (instance? Delete fact)
     (let [fact (:record fact)]
       (rules/retract! fact)
       (when (and (instance? Def fact)
                  (record? (:value fact)))
         (rules/retract! (:value fact))))
     (do
       (rules/insert! fact)
       (when (and (instance? Def fact)
                  (record? (:value fact)))
         (rules/insert! (:value fact))))))
  ([session fact]
   (when (instance? Delete fact)
     (throw (js/Error. "Can't delete here.")))
   (cond-> session
           true
           (rules/insert fact)
           (and (instance? Def fact)
                (record? (:value fact)))
           (rules/insert (:value fact)))))

(defn watch-files [files]
  (when-not js/COMPILED
    (.send XhrIo
      "/watch"
      (fn [])
      "POST"
      (pr-str files))))

