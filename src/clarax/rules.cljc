(ns clarax.rules
  (:require [clara.rules :as rules]
            [clara.rules.engine :as engine]
            [clara.rules.accumulators])
  (:refer-clojure :exclude [merge update]))

(defprotocol IMerge
  (merge [this fact new-args]))

(defprotocol IUpdate
  (update [this fact k f]
          [this fact k f x]
          [this fact k f x y]
          [this fact k f x y z]
          [this fact k f x y z & more]))

(defn- update-fact [session old-fact new-fact]
  (-> session
      (rules/retract old-fact)
      (rules/insert new-fact)))

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
        (update-fact fact (clojure.core/merge fact new-args))
        rules/fire-rules
        (Session. queries query-fns)))
  
  IUpdate
  (update [this fact k f]
    (-> session
        (update-fact fact (clojure.core/update fact k f))
        rules/fire-rules
        (Session. queries query-fns)))
  (update [this fact k f x]
    (-> session
        (update-fact fact (clojure.core/update fact k f x))
        rules/fire-rules
        (Session. queries query-fns)))
  (update [this fact k f x y]
    (-> session
        (update-fact fact (clojure.core/update fact k f x y))
        rules/fire-rules
        (Session. queries query-fns)))
  (update [this fact k f x y z]
    (-> session
        (update-fact fact (clojure.core/update fact k f x y z))
        rules/fire-rules
        (Session. queries query-fns)))
  (update [this fact k f x y z & more]
    (-> session
        (update-fact fact (apply clojure.core/update fact k f x y z more))
        rules/fire-rules
        (Session. queries query-fns))))

(defn- update-fact! [old-fact new-fact]
  (rules/retract! old-fact)
  (rules/insert-unconditional! new-fact))

(defn merge! [fact new-args]
  (update-fact! fact (clojure.core/merge fact new-args)))

(defn update!
  ([fact k f]
   (update-fact! fact (clojure.core/update fact k f)))
  ([fact k f x]
   (update-fact! fact (clojure.core/update fact k f x)))
  ([fact k f x y]
   (update-fact! fact (clojure.core/update fact k f x y)))
  ([fact k f x y z]
   (update-fact! fact (clojure.core/update fact k f x y z)))
  ([fact k f x y z & more]
   (update-fact! fact (apply clojure.core/update fact k f x y z more))))

