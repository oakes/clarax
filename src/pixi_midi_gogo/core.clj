(ns pixi-midi-gogo.core
  (:require [clara.macros :as macros]
            [cljs.env :as env]
            [clara.rules.dsl :as dsl]))

(defn add-rule [ns-sym name body]
  (->> (dsl/build-rule name body)
       (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym name])))

(defn read-rules-for-ns [ns-sym rules]
  (doseq [i (range (count rules))
          :let [{:keys [on do]} (get rules i)
                sym (symbol (str "rule-" i))]]
    (add-rule ns-sym sym (list on '=> do)))
  (macros/sources-and-options->session-assembly-form (list (list 'quote ns-sym))))

(defmacro read-rules [& rules]
  (reduce
    (fn [m [ns-sym rules]]
      (assoc m (list 'quote ns-sym)
        (read-rules-for-ns ns-sym rules)))
    {}
    (group-by :in rules)))

