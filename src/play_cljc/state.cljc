(ns play-cljc.state
  (:require [clara.rules :as rules]
            [clara.rules.engine :as engine]
            [clara.rules.accumulators]))

(defn- check-for-context []
  (when-not engine/*rule-context*
    (throw (ex-info "No session found. You must use the other arity of this function." {}))))

(defn- inc-version [fact]
  (if-let [*version (:*version fact)]
    (assoc fact :version (swap! *version inc))
    fact))

(defn insert!
  ([fact]
   (check-for-context)
   (rules/insert! (inc-version fact)))
  ([state fact]
   (if engine/*rule-context*
     (do (insert! fact) state)
     (update state :session (fn [session]
                              (-> session
                                  (rules/insert (inc-version fact))
                                  rules/fire-rules))))))

(defn delete!
  ([fact]
   (check-for-context)
   (rules/retract! fact))
  ([state fact]
   (if engine/*rule-context*
     (do (delete! fact) state)
     (update state :session (fn [session]
                              (-> session
                                  (rules/retract fact)
                                  rules/fire-rules))))))

(defn update!
  ([fact new-args]
   (check-for-context)
   (rules/retract! fact)
   (insert! (merge fact new-args)))
  ([state fact new-args]
   (if engine/*rule-context*
     (do (update! fact new-args) state)
     (-> state
         (delete! fact)
         (insert! (merge fact new-args))))))

(defn query
  ([state q]
   (query state q {}))
  ([state q params]
   (some-> state
           :session
           (engine/query q params)
           first
           :?ret)))

(defn query-fact [state fact-name]
  (query state (or (get (:queries state) fact-name)
                   (throw (ex-info (str "Query for " fact-name " not found") {})))))

