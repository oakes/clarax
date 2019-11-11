(ns play-cljc.state.macros-js
  (:require [play-cljc.state.build :as build]
            [clara.macros :as macros]
            [clara.rules :as rules]))

(defmacro ->session [& rules-and-queries]
  (-> @build/*productions
      (select-keys rules-and-queries)
      vals vec eval
      (macros/productions->session-assembly-form [])))

(defmacro defquery [& form]
  (let [sym (first form)
        query (build/form->query form)]
    (binding [*out* (java.io.StringWriter.)]
      (build/add-production sym query))
    `(def ~sym ~query)))

(defmacro defrule [& form]
  (let [sym (first form)
        rule (build/form->rule form)]
    (binding [*out* (java.io.StringWriter.)]
      (build/add-production sym rule))
    `(def ~sym ~rule)))

