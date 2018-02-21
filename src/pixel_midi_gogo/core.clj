(ns pixel-midi-gogo.core
  (:require [clara.macros :as macros]
            [clara.rules :as rules]
            [cljs.env :as env]
            [clara.rules.dsl :as dsl]
            [clojure.java.io :as io]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :refer [indexing-push-back-reader]]))

(def ^:const ns-sym 'pixel-midi-gogo.core)

(def default-rules
  '[{:select [[?facts <- (clara.rules.accumulators/all)
               :from [pixel-midi-gogo.core/Def (= ?id id) (some? id)]]]
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

(defn read-file [filename]
  (let [reader (indexing-push-back-reader (slurp (io/reader filename)))]
    (loop [forms []]
      (if-let [form (r/read {:eof nil} reader)]
        (recur (conj forms form))
        forms))))

(defn read-init-forms [forms]
  (->> forms
       (take-while #(not (keyword? %)))
       (map #(list 'pixel-midi-gogo.core/insert %))))

(defn read-rules [forms]
  (->> forms
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
         default-rules)))

(defmacro init [nses files]
  (let [forms (mapv read-file files)
        init-forms (vec (mapcat read-init-forms forms))
        rules (vec (mapcat read-rules forms))
        _ (add-rules rules)
        session (macros/sources-and-options->session-assembly-form
                  (map #(list 'quote %) nses))]
    `(reset! pixel-midi-gogo.core/*session
       (-> ~session ~@init-forms rules/fire-rules))))

