(ns play-cljc.state
  (:require [clara.rules :as rules]
            [clara.rules.engine :as engine]
            [clara.rules.accumulators])
  (:refer-clojure :exclude [merge]))

(defprotocol IMerge
  (merge [this fact new-args]))

(deftype Session [session queries query-fns]
  engine/ISession
  (insert [this facts]
    (-> (engine/insert session facts)
        rules/fire-rules
        (Session. queries query-fns)))
  (retract [this facts]
    (-> (engine/retract session facts)
        rules/fire-rules
        (Session. queries query-fns)))
  (fire-rules [this]
    (-> (engine/fire-rules session)
        (Session. queries query-fns)))
  (fire-rules [this opts]
    (-> (engine/fire-rules session opts)
        (Session. queries query-fns)))
  (query [this query params]
    (some-> session
            (engine/query (or (get queries query) query) params)
            first
            ((or (get query-fns query) identity))))
  (components [this]
    (engine/components session))
  
  IMerge
  (merge [this fact new-args]
    (-> session
        (rules/retract fact)
        (rules/insert (clojure.core/merge fact new-args))
        rules/fire-rules
        (Session. queries query-fns))))

(defn merge! [fact new-args]
  (rules/retract! fact)
  (rules/insert-unconditional! (clojure.core/merge fact new-args)))

