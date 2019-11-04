(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]
            [clara.rules.engine :as eng]))

(defmacro ->session [& forms]
  (let [{:keys [init-forms rules queries]} (build/forms->rules forms)
        rules (into rules (vals queries))]
    `(do
       ~(cons 'do
          (for [[sym query] queries]
            `(def ~sym ~query)))
       (->> ~rules compiler/mk-session ~@init-forms rules/fire-rules))))

(defmacro query [session query & params]
  `(some-> ~session
           (eng/query ~query ~(apply hash-map params))
           first
           :?ret))

