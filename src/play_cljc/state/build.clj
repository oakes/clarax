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

(defn build-query [{:keys [record args]}]
  (vec (cons
         record
         (map (fn [{:keys [key val]}]
                (let [key (symbol (name key))]
                  (if (list? val)
                    (walk/prewalk-replace {'% key} val)
                    (list '= key val))))
           args))))

(defn transform-when-form [{:keys [binding] :as form}]
  (let [query (build-query form)]
    (if-let [{:keys [symbol arrow]} binding]
      (case arrow
        <<- [symbol '<- '(clara.rules.accumulators/distinct)
             :from query]
        <- (vec (concat [symbol '<-] query)))
      query)))

(defn select-form->query [{:keys [binding] :as form}]
  (dsl/build-query (:symbol binding)
    (list [] ['?ret '<-
              (case (:arrow binding)
                <<- '(clara.rules.accumulators/distinct)
                <- '(clara.rules.accumulators/max :timestamp :returns-fact true))
              :from (build-query form)])))

(defn build-rule [{:keys [name left right]}]
  (dsl/build-rule name
    (concat
      (map transform-when-form (:forms left))
      ['=>]
      (:forms right))))

(s/def ::binding (s/cat
                   :symbol symbol?
                   :arrow '#{<- <<-}))
(s/def ::pair (s/cat
                :key keyword?
                :val any?))

(s/def ::when-form (s/cat
                     :binding (s/? ::binding)
                     :record symbol?
                     :args (s/* ::pair)))
(s/def ::select-form (s/cat
                       :binding ::binding
                       :record symbol?
                       :args (s/* ::pair)))

(s/def ::when-block (s/cat
                      :header #{:when}
                      :forms (s/+ (s/spec ::when-form))))
(s/def ::execute-block (s/cat
                         :header #{:execute}
                         :forms (s/+ list?)))

(s/def ::rule (s/cat
                :name symbol?
                :left ::when-block
                :right ::execute-block))

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
       (parse ::select-form)
       select-form->query))

(defn form->rule [form]
  (->> form
       (parse ::rule)
       build-rule))

