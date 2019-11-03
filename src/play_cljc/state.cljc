(ns play-cljc.state
  (:require [clara.rules :as rules]
            [clara.rules.engine :as engine]
            [clara.rules.accumulators]))

(defprotocol Insertable
  (insert! [this] [this session]))

(defprotocol Deletable
  (delete! [this] [this session]))

(defprotocol Updatable
  (update! [this new-args] [this new-args session]))

(defprotocol ActionReceivable
  (receive-action! [this action-name]))

(defprotocol ActionSendable
  (send-action! [this action-name]))

(defn insert*
  ([fact]
   (rules/insert! fact))
  ([fact session]
   (if engine/*rule-context*
     (do (insert* fact) session)
     (-> session
         (rules/insert fact)
         rules/fire-rules))))

(defn delete*
  ([fact]
   (rules/retract! fact))
  ([fact session]
   (if engine/*rule-context*
     (do (delete* fact) session)
     (-> session
         (rules/retract fact)
         rules/fire-rules))))

(defn update*
  ([fact new-args]
   (rules/retract! fact)
   (insert! (merge fact new-args)))
  ([fact new-args session]
   (if engine/*rule-context*
     (do (update* fact new-args) session)
     (->> (rules/retract session fact)
          (insert! (merge fact new-args))))))

(extend-type #?(:clj Object :cljs default)
  Insertable
  (insert!
    ([this] (insert* this))
    ([this session] (insert* this session)))
  Deletable
  (delete!
    ([this] (delete* this))
    ([this session] (delete* this session)))
  Updatable
  (update!
    ([this new-args] (update* this new-args))
    ([this new-args session] (update* this new-args session))))

(defn get-time []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

