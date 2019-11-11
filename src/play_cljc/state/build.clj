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
            [clojure.walk :as walk]))

(def *productions (atom {}))

(defn add-production [sym prod]
  (swap! *productions update sym (fn [existing-prod]
                                   (when (and existing-prod
                                              (not= existing-prod prod))
                                     (println "WARNING:" sym "has been redefined"))
                                   prod)))

(defn transform-when-form [{:keys [binding record args]}]
  (let [query (into [record] args)]
    (if-let [{:keys [symbol arrow]} binding]
      (case arrow
        <<- [symbol '<- '(clara.rules.accumulators/distinct)
             :from query]
        <- (vec (concat [symbol '<-] query)))
      query)))

(defn select-form->query [{:keys [binding record args]}]
  (dsl/build-query (:symbol binding)
    (list [] ['?ret '<-
              (case (:arrow binding)
                <<- '(clara.rules.accumulators/distinct)
                <- '(clara.rules.accumulators/max :version :returns-fact true))
              :from (into [record] args)])))

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

(defn form->rule [form]
  (->> form
       (parse ::rule)
       build-rule))

