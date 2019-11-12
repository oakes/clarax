(ns play-cljc.state.macros-js
  (:require [play-cljc.state.build :as build]
            [clara.macros :as macros]
            [clara.rules :as rules]))

(defmacro ->session [& fact-names]
  (-> fact-names
      build/get-productions-for-facts
      eval
      (macros/productions->session-assembly-form [])))

(defmacro ->fact [name & args]
  (build/->fact* name args))

(defmacro deffact [name fields & opts]
  (build/deffact* name fields opts))

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

