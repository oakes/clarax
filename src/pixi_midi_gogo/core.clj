(ns pixi-midi-gogo.core
  (:require [clara.macros :as macros]
            [cljs.env :as env]
            [clara.rules.dsl :as dsl]))

(defn add-rule [ns-sym name body]
  (->> (dsl/build-rule name body)
       (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym name]))
  nil)

(defn read-rules-for-ns [[ns-sym rules]]
  (doseq [i (range (count rules))
          :let [{:keys [on do]} (get rules i)
                sym (symbol (str "rule-" i))]]
    (add-rule ns-sym sym (list on '=> do))))

(defmacro read-rules [& rules]
  (let [rules (reduce
                (fn [rules rule]
                  (conj rules
                    (if-not (:in rule)
                      (assoc rule :in 'pixi-midi-gogo.core)
                      rule)))
                []
                rules)
        init-forms (->> rules
                        (remove :on)
                        (map :do))
        ns->rules (->> rules
                       (filter :on)
                       (group-by :in))
        _ (run! read-rules-for-ns ns->rules)
        session (macros/sources-and-options->session-assembly-form
                  (for [ns-sym (keys ns->rules)]
                    (list 'quote ns-sym)))]
    `(-> ~session ~@init-forms)))

