(ns clarax.parse
  (:require [clarax.rules]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]
            [clara.rules.dsl :as dsl]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.walk :as walk]
            [clojure.string :as str]))

(s/def ::as symbol?)

(def ^:private starts-with-?
  #(str/starts-with? (str %) "?"))

(s/def ::let-left (s/or
                    :simple (s/and symbol? #(not (starts-with-? %)))
                    :destructure (s/keys :req-un [::as])))

(s/def ::let-right (s/or
                     :latest symbol?
                     ;; this is the old syntax for accumulators
                     :all (s/tuple symbol?)
                     :accumulator (s/tuple symbol? any?)))

(s/def ::when-form (s/cat
                     :key #{:when}
                     :val any?))

(s/def ::accumulator-form (s/cat
                            :key #{:accumulator}
                            :val any?))

(s/def ::let-pair (s/cat
                    :left ::let-left
                    :right ::let-right
                    :opts (s/* (s/alt
                                 :when ::when-form
                                 :accumulator ::accumulator-form))))

(s/def ::let-form (s/cat
                    :sym '#{let}
                    :bindings (s/spec (s/* ::let-pair))
                    :body (s/* any?)))

(s/def ::fn-form (s/cat
                   :sym '#{fn}
                   :args (s/coll-of (s/and symbol? starts-with-?)
                                    :kind vector?)
                   :body (s/spec ::let-form)))

(s/def ::body (s/map-of keyword?
                        (s/or :query ::fn-form
                              :rule ::let-form)))

(defn parse [spec content]
  (let [res (s/conform spec content)]
    (if (= ::s/invalid res)
      (throw (ex-info (expound/expound-str spec content) {}))
      res)))

(extend-type java.util.Map
  compiler/IRuleSource
  (load-rules [m]
    [m]))

(defn get-symbol [[kind value]]
  (case kind
    :simple value
    :destructure (:as value)))

(defn get-binding-symbol [sym]
  (symbol (str '? sym)))

(defn ->destructure-pairs [bindings]
  (reduce
    (fn [v {:keys [left]}]
      (let [[_ value] left
            sym (get-symbol left)]
        (conj v [value (get-binding-symbol sym)])))
    []
    bindings))

(defn ->destructure-map [pairs]
  (reduce
    (fn [m [k v]]
      (assoc m k (keyword v)))
    {}
    pairs))

(defn binding-symbol? [x]
  (and (symbol? x) (starts-with-? x)))

(defn wrap-in-let [bindings body]
  (if (and (seq bindings)
           ;; binding symbols may be query args,
           ;; and wrapping them in a `let` can break them
           (not (binding-symbol? body)))
    (list 'let
      (reduce into (->destructure-pairs bindings))
      body)
    body))

(defn ->conditions [bindings opts]
  (let [condition (:when opts)
        conditions (cond
                     (nil? condition) []
                     (and (list? condition) (= 'and (first condition))) (drop 1 condition)
                     :else [condition])]
    (mapv (fn [condition]
            (if (= '= (first condition))
              (->> (rest condition)
                   (map (partial wrap-in-let bindings))
                   (cons '=))
              (wrap-in-let bindings condition)))
          conditions)))

(defn opts->map [opts]
  (reduce
    (fn [m [_ {:keys [key val] :as opt}]]
      (when (contains? m key)
        (throw (ex-info (str "Duplicate key " key) {})))
      (assoc m key val))
    {}
    opts))

(defn transform-let-binding [bindings {:keys [left right opts] :as bind}]
  (let [sym (get-symbol left)
        destructure-sym (second left)
        binding-sym (get-binding-symbol sym)
        opts (opts->map opts)
        conditions (->conditions bindings opts)
        [right-kind right-value] right]
    (when (and (:accumulator opts)
               (#{:all :accumulator} right-kind))
      (throw (ex-info "You cannot use the old and new accumulator syntax at the same time" bind)))
    (case right-kind
      :latest
      (if-let [acc (:accumulator opts)]
        [binding-sym '<- acc
         :from (into [right-value [destructure-sym]]
                     conditions)]
        (into [binding-sym '<-]
              (into [right-value [destructure-sym]]
                    conditions)))
      ;; this is the old syntax for accumulators
      [binding-sym '<- (case right-kind
                         :all '(clara.rules.accumulators/all)
                         :accumulator (second right-value))
       :from (into [(first right-value) [destructure-sym]]
                   conditions)])))

(defn transform-let-bindings [bindings]
  (:ret
    (reduce
      (fn [m binding]
        (-> m
            (update :ret conj
                    (transform-let-binding (:bindings m) binding))
            (update :bindings conj binding)))
      {:ret []
       :bindings []}
      bindings)))

(defn build-query [name fn-form]
  (->> fn-form
       :body
       :bindings
       transform-let-bindings
       (cons (->> fn-form :args (mapv keyword)))
       (dsl/build-query (symbol name))))

(defn build-query-fn [fn-form query]
  `(fn query-fn#
     ([session#]
      (query-fn# session# {}))
     ([session# params#]
      (when-let [ret# (first (clara.rules.engine/query session# ~query params#))]
        (let [{:keys ~(:args fn-form)} params#
              ~(-> fn-form
                   :body
                   :bindings
                   ->destructure-pairs
                   ->destructure-map) ret#]
          ~@(-> fn-form :body :body))))))

(defn build-rule [name {:keys [bindings body]} platform]
  (dsl/build-rule (symbol name)
    (concat
      (transform-let-bindings bindings)
      ['=>]
      (list
        (as->
          (concat
            (list 'let (reduce into (->destructure-pairs bindings)))
            body)
          $
          (case platform
            ;; for some reason, exceptions in rules sometimes don't print anything out,
            ;; so here we are catching them and forcing them to print out
            :clj (list 'try $ '(catch Exception e (.printStackTrace e) (throw e)))
            :cljs (list 'try $ '(catch js/Error e (js/console.error (.-message e)) (throw e)))
            $))))))

(defn ->productions [body {:keys [platform]}]
  (let [*queries (volatile! {})
        *query-fns (volatile! {})
        *rules (volatile! {})
        parsed-body (parse ::body body)
        _ (doseq [[name [kind prod]] parsed-body]
            (case kind
              :query (let [query (build-query name prod)]
                       (vswap! *queries assoc name query)
                       (vswap! *query-fns assoc name (build-query-fn prod query)))
              :rule (vswap! *rules assoc name (build-rule name prod platform))))
        queries @*queries
        productions (-> (vec (vals @*rules))
                        (into (vals queries)))]
    {:productions productions
     :queries queries
     :query-fns @*query-fns}))

