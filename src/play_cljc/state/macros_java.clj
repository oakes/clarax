(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defn ->session [& rules-and-queries]
  (compiler/mk-session rules-and-queries))

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

