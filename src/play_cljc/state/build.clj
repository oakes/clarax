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

(s/def ::query-form (s/cat
                      :symbol symbol?
                      :arrow '#{<- <<-}
                      :record symbol?
                      :args (s/* any?)))

(s/def ::left-side (s/+ (s/spec ::query-form)))

(s/def ::right-side (s/+ list?))

(s/def ::rule (s/cat
                :header #{:rule}
                :symbol symbol?
                :left ::left-side
                :split '#{=>}
                :right ::right-side))

(s/def ::query (s/cat
                 :header #{:query}
                 :symbol symbol?
                 :arrow '#{<- <<-}
                 :record symbol?
                 :args (s/* any?)))

(s/def ::body (s/coll-of (s/or :query ::query
                               :rule ::rule)))

(defn parse [spec content]
  (let [res (s/conform spec content)]
    (if (= ::s/invalid res)
      (throw (ex-info (expound/expound-str spec content) {}))
      res)))

(defn parse-body [body]
  (parse ::body body))

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
  (swap! *facts* conj record)
  (let [query (-> (into [record] args)
                  (into ['(= version @*version)]))]
    (if (= arrow '<-)
      (vec (concat [symbol '<-] query))
      [symbol '<- '(clara.rules.accumulators/distinct)
       :from query])))

(defn select-form->query [{:keys [symbol arrow record args]}]
  (swap! *facts* conj record)
  (dsl/build-query symbol
    (list [] ['?ret '<-
              (if (= arrow '<-)
                '(clara.rules.accumulators/max :version :returns-fact true)
                '(clara.rules.accumulators/distinct))
              :from (into [record] args)])))

(defn build-rule [{:keys [symbol left right]}]
  (dsl/build-rule symbol
    (concat
      (map transform-when-form left)
      ['=>]
      right)))

(defn get-state [body]
  (binding [*facts* (atom #{})]
    (let [*queries (volatile! {})
          *rules (volatile! {})
          parsed-body (parse-body body)
          _ (doseq [[kind prod] parsed-body]
              (case kind
                :query (vswap! *queries assoc (:symbol prod) (select-form->query prod))
                :rule (vswap! *rules assoc (:symbol prod) (build-rule prod))))
          fact-names @*facts*
          fact-queries (get-fact-queries fact-names)
          queries (reduce-kv
                    (fn [m k v]
                      (assoc m (list 'quote k) v))
                    {}
                    @*queries)
          delete-rules (get-delete-rules fact-names)
          productions (-> (vec (vals @*rules))
                          (into delete-rules)
                          (into (vals queries))
                          (into (vals fact-queries)))]
      {:productions productions
       :queries (merge queries fact-queries)})))

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

