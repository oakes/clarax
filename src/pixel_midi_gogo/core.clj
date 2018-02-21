(ns pixel-midi-gogo.core
  (:require [clara.macros :as macros]
            [clara.rules :as rules]
            [cljs.env :as env]
            [clara.rules.dsl :as dsl]))

(def ^:const ns-sym 'pixel-midi-gogo.core)

(def default-rules
  '[{:select [[?facts <- (clara.rules.accumulators/all)
               :from [pixel-midi-gogo.core/Fact (= ?id id) (some? id)]]]
     :execute [(pixel-midi-gogo.core/delete ?facts)]}])

(defn add-rule [name body]
  (->> (dsl/build-rule name body)
       (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym name])))

(defn add-rules [rules]
  (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym] {})
  (doseq [i (range (count rules))
          :let [{:keys [select insert execute]} (get rules i)
                sym (symbol (str "rule-" i))]]
    (add-rule sym (concat select ['=>] execute
                    (map #(list 'pixel-midi-gogo.core/insert %)
                      insert)))))

(defmacro read-rules [nses & rules]
  (let [init-forms (->> rules
                        (take-while #(not (keyword? %)))
                        (map #(list 'pixel-midi-gogo.core/insert %)))
        rules (->> rules
                   (drop-while #(not (keyword? %)))
                   (partition-by keyword?)
                   (partition 2)
                   (reduce
                     (fn [new-rules [header code]]
                       (if (= (first header) :select)
                         (conj new-rules {:select code})
                         (if (seq new-rules)
                           (update new-rules (dec (count new-rules))
                             assoc (first header) code))))
                     default-rules))
        _ (add-rules rules)
        session (macros/sources-and-options->session-assembly-form
                  (map #(list 'quote %) nses))]
    `(reset! pixel-midi-gogo.core/*session
       (-> ~session ~@init-forms rules/fire-rules))))

