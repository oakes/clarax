(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defmacro ->session [& fact-names]
  `(compiler/mk-session ~(build/get-productions-for-facts fact-names)))

(defmacro deffact [name fields & opts]
  (build/deffact* name fields opts))

(defmacro ->fact [name & args]
  (build/->fact* name args))

(defmacro defquery [& form]
  (let [sym (first form)
        query (build/form->query form)]
    (build/add-production sym query)
    `(def ~sym ~query)))

(defmacro defrule [& form]
  (let [sym (first form)
        rule (build/form->rule form)]
    (build/add-production sym rule)
    `(def ~sym ~rule)))

