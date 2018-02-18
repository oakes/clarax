(ns pixi-midi-gogo.core
  (:require [clara.macros :as macros]
            [cljs.env :as env]
            [clara.rules.dsl :as dsl]))

(def ^:const ns-sym 'pixi-midi-gogo.core)

(defn add-rule [name body]
  (->> (dsl/build-rule name body)
       (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym name]))
  nil)

(defn add-rules [rules]
  (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym] {})
  (doseq [i (range (count rules))
          :let [{:keys [on do]} (get rules i)
                sym (symbol (str "rule-" i))]]
    (add-rule sym (list on '=> do))))

(defmacro read-rules [nses & rules]
  (let [init-forms (->> rules
                        (remove :on)
                        (map :do))
        _ (add-rules (filterv :on rules))
        session (macros/sources-and-options->session-assembly-form
                  (map #(list 'quote %) nses))]
    `(-> ~session ~@init-forms)))

