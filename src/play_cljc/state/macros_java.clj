(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defmacro defsession [var-name & forms]
  (let [{:keys [init-forms rules]} (build/forms->rules var-name forms)]
    `(let [var# (def ~var-name (atom nil))]
       (->> ~rules compiler/mk-session ~@init-forms rules/fire-rules (reset! ~var-name))
       var#)))

