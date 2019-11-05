(ns play-cljc.state.macros-js
  (:require [play-cljc.state.build :as build]
            [clara.macros :as macros]
            [clara.rules :as rules]))

(def ^:private *productions (atom {}))

(defmacro ->session [& rules-and-queries]
  (-> @*productions
      (select-keys rules-and-queries)
      vals vec eval
      (macros/productions->session-assembly-form [])))

(defmacro defquery [& form]
  (let [sym (first form)
        query (build/form->query form)]
    (swap! *productions assoc sym query)
    `(def ~sym ~query)))

(defmacro defrule [& form]
  (let [sym (first form)
        rule (build/form->rule form)]
    (swap! *productions assoc sym rule)
    `(def ~sym ~rule)))

