(ns pixel-midi-gogo.core
  (:require [clara.rules :as rules])
  (:import goog.net.XhrIo))

(def *session (atom nil))

(defn insert
  ([fact]
   (rules/insert! fact))
  ([session fact]
   (rules/insert session fact)))

(defn delete [fact]
  (rules/retract! fact))

(defn edit
  ([fact new-args]
   (rules/retract! fact)
   (rules/insert! (merge fact new-args)))
  ([session fact new-args]
   (-> session
       (rules/retract fact)
       (rules/insert (merge fact new-args)))))

(defn watch-files [files]
  (when-not js/COMPILED
    (.send XhrIo
      "/watch"
      (fn [])
      "POST"
      (pr-str files))))

