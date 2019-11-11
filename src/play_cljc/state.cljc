(ns play-cljc.state
  (:require [clara.rules :as rules]
            [clara.rules.engine :as engine]
            [clara.rules.accumulators]))

(defn insert!
  ([fact]
   (rules/insert! fact))
  ([session fact]
   (if engine/*rule-context*
     (do (insert! fact) session)
     (-> session
         (rules/insert fact)
         rules/fire-rules))))

(defn delete!
  ([fact]
   (rules/retract! fact))
  ([session fact]
   (if engine/*rule-context*
     (do (delete! fact) session)
     (-> session
         (rules/retract fact)
         rules/fire-rules))))

(defn delete-all! [session facts]
  (->> facts
       (reduce rules/retract session)
       rules/fire-rules))

(defn update!
  ([fact new-args]
   (rules/retract! fact)
   (insert! (merge fact new-args)))
  ([session fact new-args]
   (if engine/*rule-context*
     (do (update! fact new-args) session)
     (-> (rules/retract session fact)
         (insert! (merge fact new-args))))))

(defn query
  ([session q]
   (query session q {}))
  ([session q params]
   (some-> session
           (engine/query q params)
           first
           :?ret)))

