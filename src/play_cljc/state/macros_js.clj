(ns play-cljc.state.macros-js
  (:require [play-cljc.state.build :as build]
            [clara.macros :as macros]
            [clara.rules :as rules]))

(defmacro deffact [name fields & opts]
  (binding [build/*macro-name* (first &form)
            *out* (java.io.StringWriter.)]
    (build/deffact* name fields opts)))

(defmacro defquery [& form]
  (binding [build/*macro-name* (first &form)
            *out* (java.io.StringWriter.)]
    (build/defquery* form)))

(defmacro defrule [& form]
  (binding [build/*macro-name* (first &form)
            *out* (java.io.StringWriter.)]
    (build/defrule* form)))

(defmacro ->state []
  (let [fact-names (build/get-fact-names)
        productions (build/get-productions-for-facts fact-names)
        fact-queries (build/get-fact-queries fact-names)]
    {:session (-> (into productions (vals fact-queries))
                  eval
                  (macros/productions->session-assembly-form []))
     :queries fact-queries}))

(defmacro ->fact [name & args]
  (build/->fact* name args))

