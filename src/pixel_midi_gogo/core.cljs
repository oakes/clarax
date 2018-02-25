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

(defn watch-files [files]
  (when-not js/COMPILED
    (.send XhrIo
      "/watch"
      (fn [])
      "POST"
      (pr-str files))))

