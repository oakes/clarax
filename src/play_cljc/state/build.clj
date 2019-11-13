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

(def ^:dynamic *rules* (atom {}))
(def ^:dynamic *queries* (atom {}))
(def ^:dynamic *macro-name* nil)

(defn get-delete-rules [fact-names]
  (reduce
    (fn [productions fact-name]
      (conj productions
            (dsl/build-rule (symbol (str 'delete- fact-name))
              [['?fact '<- fact-name '(< version @*version)]
               '=> '(play-cljc.state/delete! ?fact)])))
    []
    fact-names))

(defn get-rules []
  (-> @*rules* vals vec))

(defn get-queries []
  (-> @*queries* vals vec))

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

(defn get-state [fact-names]
  (let [fact-queries (get-fact-queries fact-names)
        queries (get-queries)
        delete-rules (get-delete-rules fact-names)
        productions (-> (get-rules)
                        (into delete-rules)
                        (into queries)
                        (into (vals fact-queries)))]
    {:productions productions
     :queries fact-queries}))

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

(defn transform-when-form [{:keys [binding record args]}]
  (let [query (-> (into [record] args)
                  (into ['(= version @*version)]))]
    (if-let [sym (:symbol binding)]
      (if (= (:arrow binding) '<-)
        (vec (concat [sym '<-] query))
        [sym '<- '(clara.rules.accumulators/distinct)
         :from query])
      query)))

(defn select-form->query [{:keys [binding record args]}]
  (let [sym (:symbol binding)]
    (dsl/build-query sym
      (list [] ['?ret '<-
                (if (= (:arrow binding) '<-)
                  '(clara.rules.accumulators/max :version :returns-fact true)
                  '(clara.rules.accumulators/distinct))
                :from (into [record] args)]))))

(defn build-rule [{:keys [name left right]}]
  (dsl/build-rule name
    (concat
      (map transform-when-form left)
      ['=>]
      right)))

(s/def ::binding (s/cat
                   :symbol symbol?
                   :arrow '#{<- <<-}))

(s/def ::query-form (s/cat
                      :binding ::binding
                      :record symbol?
                      :args (s/* any?)))

(s/def ::left-side (s/+ (s/spec ::query-form)))

(s/def ::right-side (s/+ list?))

(s/def ::rule (s/cat
                :name symbol?
                :left ::left-side
                :split '#{=>}
                :right ::right-side))

(defn parse [spec content]
  (let [res (s/conform spec content)]
    (if (= ::s/invalid res)
      (throw (ex-info (expound/expound-str spec content) {}))
      res)))

(extend-type java.util.Map
  compiler/IRuleSource
  (load-rules [m]
    [m]))

(defn form->query [form]
  (->> form
       (parse ::query-form)
       select-form->query))

(defn defquery* [form]
  (let [sym (first form)
        query (form->query form)]
    (swap! *queries* assoc sym query)
    `(def ~sym ~query)))

(defn form->rule [form]
  (->> form
       (parse ::rule)
       build-rule))

(defn defrule* [form]
  (let [sym (first form)
        rule (form->rule form)]
    (swap! *rules* assoc sym rule)
    `(def ~sym ~rule)))

