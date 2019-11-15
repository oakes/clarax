(ns play-cljc.state
  (:require [clara.rules :as rules]
            [clara.rules.engine :as engine]
            [clara.rules.accumulators])
  (:refer-clojure :exclude [merge]))

(defn- check-for-context []
  (when-not engine/*rule-context*
    (throw (ex-info "No session found. You must use the other arity of this function." {}))))

(defn insert
  ([fact]
   (check-for-context)
   (rules/insert! fact))
  ([state fact]
   (if engine/*rule-context*
     (do (insert fact) state)
     (update state :session
             (fn [session]
               (-> session
                   (rules/insert fact)
                   rules/fire-rules))))))

(defn delete
  ([fact]
   (check-for-context)
   (rules/retract! fact))
  ([state fact]
   (if engine/*rule-context*
     (do (delete fact) state)
     (update state :session
             (fn [session]
               (-> session
                   (rules/retract fact)
                   rules/fire-rules))))))

(defn merge
  ([fact new-args]
   (check-for-context)
   (rules/retract! fact)
   (rules/insert-unconditional! (clojure.core/merge fact new-args)))
  ([state fact new-args]
   (if engine/*rule-context*
     (do (merge fact new-args) state)
     (-> state
         (delete fact)
         (insert (clojure.core/merge fact new-args))))))

(defn- get-query [state query-name]
  (or (get (:queries state) query-name)
      (throw (ex-info (str "Query for " query-name " not found") {}))))

(defn- get-query-fn [state query-name]
  (or (get (:query-fns state) query-name)
      (throw (ex-info (str "Query fn for " query-name " not found") {}))))

(defn query
  ([state query-name]
   (query state query-name {}))
  ([state query-name params]
   (some-> state
           :session
           (engine/query (get-query state query-name) params)
           first
           ((get-query-fn state query-name)))))

