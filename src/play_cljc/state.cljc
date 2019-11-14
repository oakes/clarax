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

(defn- get-query [state query-name]
  (or (get (:queries state) query-name)
      (throw (ex-info (str "Query for " query-name " not found") {}))))

(defn- get-query-fn [state query-name]
  (or (get (:query-fns state) query-name)
      :?ret))

(defn query
  ([state query-name]
   (query state query-name {}))
  ([state query-name params]
   (some-> state
           :session
           (engine/query (get-query state query-name) params)
           first
           ((get-query-fn state query-name)))))

