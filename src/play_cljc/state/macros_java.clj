(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]
            [clara.rules.engine :as eng]))

(defmacro defsession [var-name & forms]
  (let [{:keys [init-forms rules queries]} (build/forms->rules var-name forms)
        rules (into rules (vals queries))]
    `(let [var# (def ~var-name (atom nil))]
       ~(cons 'do
          (for [[sym query] queries]
            `(def ~sym ~query)))
       (->> ~rules compiler/mk-session ~@init-forms rules/fire-rules (reset! ~var-name))
       var#)))

(defmacro query [session query & params]
  `(-> (eng/query ~session ~query ~(apply hash-map params))
       first
       :?ret))

