(ns play-cljc.state.build
  (:require [play-cljc.state]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]
            [clara.rules.dsl :as dsl]
            [clojure.java.io :as io]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :refer [indexing-push-back-reader]]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [clojure.walk :as walk]
            [clojure.set :as set]))

(def ^:dynamic *facts* nil)
(def ^:dynamic *macro-name* nil)

(defn fact? [x]
  (when (symbol? x)
    (swap! *facts* conj x)
    true))

(s/def ::let-left (s/or
                    :simple symbol?
                    :destructure map?))

(s/def ::let-right (s/or
                     :latest fact?
                     :all (s/tuple fact?)))

(s/def ::when-form (s/cat
                     :key #{:when}
                     :condition list?))

(s/def ::let-pair (s/cat
                    :left ::let-left
                    :right ::let-right
                    :when-form (s/? ::when-form)))

(s/def ::let-form (s/cat
                    :sym '#{let}
                    :binding (s/spec (s/* ::let-pair))
                    :body (s/* any?)))

(s/def ::fn-form (s/cat
                   :sym '#{fn}
                   :args vector?
                   :body (s/spec ::let-form)))

(s/def ::query-form (s/cat
                      :symbol symbol?
                      :arrow '#{<- <<-}
                      :record fact?
                      :args (s/* any?)))

(s/def ::left-side (s/+ (s/spec ::query-form)))

(s/def ::right-side (s/+ list?))

(s/def ::rule (s/cat
                :header #{:rule}
                :left ::left-side
                :split '#{=>}
                :right ::right-side))

(s/def ::body (s/map-of keyword?
                        (s/or :rule ::rule
                              :query ::fn-form)))

(defn parse [spec content]
  (let [res (s/conform spec content)]
    (if (= ::s/invalid res)
      (throw (ex-info (expound/expound-str spec content) {}))
      res)))

(extend-type java.util.Map
  compiler/IRuleSource
  (load-rules [m]
    [m]))

(defn get-delete-rules [fact-names]
  (reduce
    (fn [productions fact-name]
      (conj productions
            (dsl/build-rule (symbol (str 'delete- fact-name))
              [['?fact '<- fact-name '(< version @*version)]
               '=> '(play-cljc.state/delete! ?fact)])))
    []
    fact-names))

(defn get-fact-queries [fact-names]
  (reduce
    (fn [m fact-name]
      (assoc m fact-name
             (dsl/build-query (symbol (str 'get- fact-name))
               (list [] ['?ret '<-
                         '(clara.rules.accumulators/max :version :returns-fact true)
                         :from [fact-name]]))))
    {}
    fact-names))

(defn transform-when-form [{:keys [symbol arrow record args]}]
  (let [query (-> (into [record] args)
                  (into ['(= version @*version)]))]
    (if (= arrow '<-)
      (vec (concat [symbol '<-] query))
      [symbol '<- '(clara.rules.accumulators/distinct)
       :from query])))

(defn get-symbol [[kind value]]
  (case kind
    :simple value))

(defn get-binding-symbol [sym]
  (symbol (str '? sym)))

(defn build-query [name fn-form]
  (->> fn-form
       :body
       :binding
       (reduce
         (fn [exprs {:keys [left right when-form]}]
           (let [sym (get-symbol left)
                 binding-sym (get-binding-symbol sym)
                 condition (:condition when-form)]
             (conj exprs
               (into
                 [binding-sym '<-]
                 (case (first right)
                   :latest ['(clara.rules.accumulators/max :version :returns-fact true)
                            :from (cond-> [(second right) [sym]]
                                          condition
                                          (conj condition))]
                   :all ['(clara.rules.accumulators/distinct)
                         :from (cond-> [(-> right second first) [sym]]
                                       condition
                                       (conj condition))])))))
         [(:args fn-form)])
       (dsl/build-query (symbol name))))

(defn destructure-symbols [bindings]
  (reduce
    (fn [m {:keys [left]}]
      (let [sym (get-symbol left)]
        (assoc m sym (-> sym get-binding-symbol keyword))))
    {}
    bindings))

(defn build-return-fn [fn-form]
  `(fn [ret#]
     (let [~(destructure-symbols (-> fn-form :body :binding)) ret#]
       ~@(-> fn-form :body :body))))

(defn build-rule [name {:keys [left right]}]
  (dsl/build-rule (symbol name)
    (concat
      (map transform-when-form left)
      ['=>]
      right)))

(defn get-state [body]
  (binding [*facts* (atom #{})]
    (let [*queries (volatile! {})
          *query-fns (volatile! {})
          *rules (volatile! {})
          parsed-body (parse ::body body)
          _ (doseq [[name [kind prod]] parsed-body]
              (case kind
                :query (do
                         (vswap! *queries assoc name (build-query name prod))
                         (vswap! *query-fns assoc name (build-return-fn prod)))
                :rule (vswap! *rules assoc name (build-rule name prod))))
          fact-names @*facts*
          fact-queries (get-fact-queries fact-names)
          queries @*queries
          delete-rules (get-delete-rules fact-names)
          productions (-> (vec (vals @*rules))
                          (into delete-rules)
                          (into (vals queries))
                          (into (vals fact-queries)))]
      {:productions productions
       :queries (merge queries fact-queries)
       :query-fns @*query-fns})))

(def ^:const reserved-fields '[version *version])

(defn deffact* [name fields opts]
  (let [invalid-fields (set/intersection (set reserved-fields) (set fields))
        fields (into reserved-fields fields)]
    (when (seq invalid-fields)
      (throw (ex-info (str name " may not contain the following reserved fields: " invalid-fields)
                      {:name name
                       :invalid-fields invalid-fields})))
    `(defrecord ~name ~fields ~@opts)))

(defn ->fact* [name args]
  `(~(symbol (str '-> name)) 0 (atom 0) ~@args))

