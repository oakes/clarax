(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defmacro ->state [& fact-names]
  (let [productions (build/get-productions-for-facts fact-names)
        fact-queries (build/get-fact-queries fact-names)]
    {:session `(compiler/mk-session ~(into productions (vals fact-queries)))
     :queries fact-queries}))

(defmacro deffact [name fields & opts]
  (build/deffact* name fields opts))

(defmacro ->fact [name & args]
  (build/->fact* name args))

(defmacro defquery [& form]
  (build/defquery* form))

(defmacro defrule [& form]
  (build/defrule* form))

