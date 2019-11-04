(ns play-cljc.state
  (:require [clara.rules :as rules]
            [clara.rules.engine :as engine]
            [clara.rules.accumulators]))

(defn insert!
  ([fact]
   (rules/insert! fact))
  ([fact session]
   (if engine/*rule-context*
     (do (insert! fact) session)
     (-> session
         (rules/insert fact)
         rules/fire-rules))))

(defn delete!
  ([fact]
   (rules/retract! fact))
  ([fact session]
   (if engine/*rule-context*
     (do (delete! fact) session)
     (-> session
         (rules/retract fact)
         rules/fire-rules))))

(defn update!
  ([fact new-args]
   (rules/retract! fact)
   (insert! (merge fact new-args)))
  ([fact new-args session]
   (if engine/*rule-context*
     (do (update! fact new-args) session)
     (->> (rules/retract session fact)
          (insert! (merge fact new-args))))))

(defn get-time []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

