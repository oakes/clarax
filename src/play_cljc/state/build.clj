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

(s/def ::let-pair (s/cat
                    :left ::let-left
                    :right ::let-right))

(s/def ::let-form (s/cat
                    :sym '#{let}
                    :binding (s/spec (s/* ::let-pair))
                    :body (s/* any?)))

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

(s/def ::query (s/or
                 :simple ::let-right
                 :let-form ::let-form))

(s/def ::body (s/map-of keyword?
                        (s/or :rule ::rule
                              :query ::query)))

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

(defn get-symbols [bindings]
  (reduce
    (fn [v {:keys [left]}]
      (conj v (get-symbol left)))
    []
    bindings))

(defn get-queries [kind value]
  (case kind
    :simple
    [{:left [:simple '?ret]
      :right value}]
    :let-form
    (:binding value)))

(defn build-query [name [kind value]]
  (->> (get-queries kind value)
       (reduce
         (fn [exprs {:keys [left right]}]
           (conj exprs
             (into
               [(get-symbol left) '<-]
               (case (first right)
                 :latest ['(clara.rules.accumulators/max :version :returns-fact true)
                          :from [(second right)]]
                 :all ['(clara.rules.accumulators/distinct)
                       :from [(-> right second first)]]))))
         [[]])
       (dsl/build-query (symbol name))))

(defn build-return-fn [[kind value]]
  (case kind
    :simple :?ret
    :let-form
    `(fn [ret#]
       (let [{:keys ~(get-symbols (:binding value))} ret#]
         ~@(:body value)))))

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

