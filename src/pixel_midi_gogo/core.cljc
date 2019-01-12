(ns pixel-midi-gogo.core
  (:require [clara.rules :as rules]
            [clara.rules.engine :as engine]
            [clara.rules.accumulators])
  #?(:cljs (:import goog.net.XhrIo)))

(defonce *session (atom nil))
(defonce *send-action-fn
  (atom (fn [action-name data]
          (throw (#?(:clj Exception. :cljs js/Error.)
                   "You must set the *send-action-fn")))))

(defn insert*
  ([fact]
   (rules/insert! fact))
  ([session fact]
   (if engine/*rule-context*
     (do (insert* fact) session)
     (-> session
         (rules/insert fact)
         rules/fire-rules))))

(defmulti insert (fn [& args]
                   (-> args last type)))

(defmethod insert
  :default
  [& args]
  (apply insert* args))

(defmulti action (fn [& args]
                   (first args)))

(defn delete
  ([fact]
   (rules/retract! fact))
  ([session fact]
   (if engine/*rule-context*
     (do (delete fact) session)
     (-> session
         (rules/retract fact)
         rules/fire-rules))))

(defn edit
  ([fact new-args]
   (rules/retract! fact)
   (rules/insert! (merge fact new-args)))
  ([session fact new-args]
   (if engine/*rule-context*
     (do (edit fact new-args) session)
     (-> session
         (rules/retract fact)
         (rules/insert (merge fact new-args))
         rules/fire-rules))))

#?(:cljs (defn watch-files [files]
           (when-not js/COMPILED
             (.send XhrIo
               "/watch"
               (fn [])
               "POST"
               (pr-str files)))))

(defn get-time []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

