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

(def ^:dynamic *productions* (atom {}))
(def ^:dynamic *facts* (atom {}))
(def ^:dynamic *production->facts* (atom {}))
(def ^:dynamic *macro-name* nil)

(defn throw-top-level-error []
  (throw (ex-info (format "%s failed" *macro-name*) {})))

(defn add-production [sym prod]
  (if *productions*
    (swap! *productions* update sym (fn [existing-prod]
                                      (when (and existing-prod
                                                 (not= existing-prod prod))
                                        (println "WARNING:" sym "has been redefined"))
                                      prod))
    (throw-top-level-error)))

(defn get-production [prod-sym]
  (if *productions*
    (get @*productions* prod-sym)
    (throw-top-level-error)))

(defn add-fact [sym fields]
  (if *facts*
    (swap! *facts* update sym (fn [existing-fact]
                                (when (and existing-fact
                                           (not= existing-fact fields))
                                  (println "WARNING:" sym "has been redefined"))
                                fields))
    (throw-top-level-error)))

(defn get-fact [fact-sym]
  (if *facts*
    (get @*facts* fact-sym)
    (throw-top-level-error)))

(defn get-fact-names []
  (if *facts*
    (vec (keys @*facts*))
    (throw-top-level-error)))

(defn add-prod->facts [prod-sym facts]
  (if *production->facts*
    (swap! *production->facts* assoc prod-sym facts)
    (throw-top-level-error)))

(defn get-facts-for-prod [prod-sym]
  (if *production->facts*
    (get @*production->facts* prod-sym)
    (throw-top-level-error)))

(defn get-production->facts []
  (if *production->facts*
    @*production->facts*
    (throw-top-level-error)))

(defn get-prod-names-for-fact [fact-name]
  (reduce-kv
    (fn [prods prod-name facts]
      (if (contains? facts fact-name)
        (conj prods prod-name)
        prods))
    #{}
    (get-production->facts)))

(defn find-missing-facts [fact-names prod-names]
  (let [required-fact-names (reduce
                              (fn [required prod-name]
                                (into required (get-facts-for-prod prod-name)))
                              #{}
                              prod-names)
        missing-fact-names (set/difference required-fact-names (set fact-names))]
    (when (seq missing-fact-names)
      (throw (ex-info (str "You must pass the following fact names to ->session: " missing-fact-names)
                      {:missing-facts missing-fact-names}))))
  prod-names)

(defn add-delete-rules [fact-names productions]
  (reduce
    (fn [productions fact-name]
      (if (get-fact fact-name)
        (conj productions
              (dsl/build-rule (symbol (str 'delete- fact-name))
                [['?fact '<- fact-name '(< version @*version)]
                 '=> '(play-cljc.state/delete! ?fact)]))
        productions))
    productions
    fact-names))

(defn get-productions-for-facts [fact-names]
  (->> fact-names
       (reduce
         (fn [prod-names fact-name]
           (into prod-names (get-prod-names-for-fact fact-name)))
         #{})
       (find-missing-facts fact-names)
       (reduce
         (fn [prods prod-name]
           (conj prods (get-production prod-name)))
         [])
       (add-delete-rules fact-names)))

(defn get-fact-queries [fact-names]
  (reduce
    (fn [m fact-name]
      (if (get-fact fact-name)
        (assoc m fact-name
               (dsl/build-query (symbol (str 'get- fact-name))
                 (list [] ['?ret '<-
                           '(clara.rules.accumulators/max :version :returns-fact true)
                           :from [fact-name]])))
        m))
    {}
    fact-names))

(def ^:const reserved-fields '[version *version])

(defn deffact* [name fields opts]
  (add-fact name fields)
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
  (let [fact? (get-fact record)
        query (cond-> (into [record] args)
                      fact?
                      (into ['(= version @*version)]))]
    (if-let [sym (:symbol binding)]
      (if (and fact? (= (:arrow binding) '<-))
        (vec (concat [sym '<-] query))
        [sym '<- '(clara.rules.accumulators/distinct)
         :from query])
      query)))

(defn select-form->query [{:keys [binding record args]}]
  (let [sym (:symbol binding)]
    (add-prod->facts sym #{record})
    (dsl/build-query sym
      (list [] ['?ret '<-
                (if (and (get-fact record)
                         (= (:arrow binding) '<-))
                  '(clara.rules.accumulators/max :version :returns-fact true)
                  '(clara.rules.accumulators/distinct))
                :from (into [record] args)]))))

(defn build-rule [{:keys [name left right]}]
  (add-prod->facts name (set (map :record left)))
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
    (add-production sym query)
    `(def ~sym ~query)))

(defn form->rule [form]
  (->> form
       (parse ::rule)
       build-rule))

(defn defrule* [form]
  (let [sym (first form)
        rule (form->rule form)]
    (add-production sym rule)
    `(def ~sym ~rule)))

