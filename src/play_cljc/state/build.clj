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
                    :bindings (s/spec (s/* ::let-pair))
                    :body (s/* any?)))

(s/def ::fn-form (s/cat
                   :sym '#{fn}
                   :args vector?
                   :body (s/spec ::let-form)))

(s/def ::body (s/map-of keyword?
                        (s/or :rule ::let-form
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

(defn get-symbol [[kind value]]
  (case kind
    :simple value))

(defn get-binding-symbol [sym]
  (symbol (str '? sym)))

(defn ->destructure-pairs [bindings]
  (reduce
    (fn [v {:keys [left]}]
      (let [sym (get-symbol left)]
        (conj v [sym (get-binding-symbol sym)])))
    []
    bindings))

(defn ->destructure-map [pairs]
  (reduce
    (fn [m [k v]]
      (assoc m k (keyword v)))
    {}
    pairs))

(defn ->condition [bindings when-form]
  (when-let [condition (:condition when-form)]
    (list 'let
          (reduce into (->destructure-pairs bindings))
          condition)))

(defn build-query [name fn-form]
  (->> fn-form
       :body
       :bindings
       (reduce
         (fn [m {:keys [left right when-form] :as binding}]
           (let [sym (get-symbol left)
                 binding-sym (get-binding-symbol sym)
                 condition (->condition (:bindings m) when-form)]
             (-> m
                 (update :ret conj
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
                                           (conj condition))])))
                 (update :bindings conj binding))))
         {:ret [(:args fn-form)]
          :bindings []})
       :ret
       (dsl/build-query (symbol name))))

(defn build-return-fn [fn-form]
  `(fn [ret#]
     (let [~(-> fn-form :body :bindings ->destructure-pairs ->destructure-map) ret#]
       ~@(-> fn-form :body :body))))

(defn transform-let-binding [bindings {:keys [left right when-form]}]
  (let [record (case (first right)
                 :latest (second right)
                 :all (-> right second first))
        sym (get-symbol left)
        binding-sym (get-binding-symbol sym)
        condition (->condition bindings when-form)
        query (cond-> [record]
                      condition
                      (conj condition)
                      true
                      (conj '(= version @*version)))]
    (case (first right)
      :latest (vec (concat [binding-sym '<-] query))
      :all [binding-sym '<- '(clara.rules.accumulators/distinct)
            :from query])))

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

(defn build-rule [name {:keys [bindings body]}]
  (dsl/build-rule (symbol name)
    (concat
      (transform-let-bindings bindings)
      ['=>]
      (list
        (concat
          (list 'let (reduce into (->destructure-pairs bindings)))
          body)))))

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
          queries @*queries
          delete-rules (get-delete-rules fact-names)
          productions (-> (vec (vals @*rules))
                          (into delete-rules)
                          (into (vals queries)))]
      {:productions productions
       :queries queries
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

