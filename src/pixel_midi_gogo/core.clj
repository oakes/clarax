(ns pixel-midi-gogo.core
  (:require [clara.macros :as macros]
            [clara.rules :as rules]
            [cljs.env :as env]
            [clara.rules.dsl :as dsl]))

(def ^:const ns-sym 'pixel-midi-gogo.core)

(def default-rules
  '[{:on [[?facts <- (clara.rules.accumulators/all)
           :from [pixel-midi-gogo.core/Fact (= ?id id) (some? id)]]]
     :do [(pixel-midi-gogo.core/delete ?facts)]}])

(defn add-rule [name body]
  (->> (dsl/build-rule name body)
       (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym name])))

(defn add-rules [rules]
  ;(swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym] {})
  (doseq [i (range (count rules))
          :let [{:keys [on do]} (get rules i)
                sym (symbol (str "rule-" i))]]
    (add-rule sym (concat on ['=>] do))))

(defmacro read-rules [nses & rules]
  (let [rules (concat default-rules rules)
        init-forms (->> rules
                        (remove :on)
                        (mapcat :do))
        _ (add-rules (filterv :on rules))
        session (macros/sources-and-options->session-assembly-form
                  (map #(list 'quote %) nses))]
    `(reset! pixel-midi-gogo.core/*session
       (-> ~session ~@init-forms rules/fire-rules))))

