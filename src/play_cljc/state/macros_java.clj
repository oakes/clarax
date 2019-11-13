(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defmacro deffact [name fields & opts]
  (binding [build/*macro-name* (first &form)]
    (build/deffact* name fields opts)))

(defmacro defquery [& form]
  (binding [build/*macro-name* (first &form)]
    (build/defquery* form)))

(defmacro defrule [& form]
  (binding [build/*macro-name* (first &form)]
    (build/defrule* form)))

(defmacro ->state []
  (let [fact-names (build/get-fact-names)
        productions (build/get-productions-for-facts fact-names)
        fact-queries (build/get-fact-queries fact-names)]
    `{:session (compiler/mk-session ~(into productions (vals fact-queries)))
      :queries ~fact-queries}))

(defmacro ->fact [name & args]
  (build/->fact* name args))

