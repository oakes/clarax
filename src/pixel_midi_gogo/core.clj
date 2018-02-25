(ns pixel-midi-gogo.core
  (:require [clara.macros :as macros]
            [clara.rules :as rules]
            [cljs.env :as env]
            [clara.rules.dsl :as dsl]
            [clojure.java.io :as io]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :refer [indexing-push-back-reader]]
            [clojure.spec.alpha :as s]))

(def ^:const ns-sym 'pixel-midi-gogo.core)

(defn add-rule [name body]
  (->> (dsl/build-rule name body)
       (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym name])))

(defn transform-insert-form [[type value]]
  (if (= type :record)
    (let [{:keys [record args]} value]
      (list
        'pixel-midi-gogo.core/insert
        (list
          (symbol (str "map->" record))
          (update
            (->> args
                 (map (juxt :key :val))
                 (into {}))
            :timestamp
            #(or % '(.getTime (js/Date.)))))))
    value))

(defn add-rules [rules]
  (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym] {})
  (doseq [i (range (count rules))
          :let [{:keys [select insert]} (get rules i)
                sym (symbol (str "rule-" i))
                right-side (map transform-insert-form (:forms insert))]]
    (add-rule sym (concat (:forms select) ['=>]
                    (if (empty? right-side) ['(do)] right-side)))))

(defn read-file [filename]
  (let [reader (indexing-push-back-reader (slurp (io/reader filename)))]
    (loop [forms []]
      (if-let [form (r/read {:eof nil} reader)]
        (recur (conj forms form))
        forms))))

(s/def ::pair (s/cat
                :key keyword?
                :val any?))
(s/def ::record-form (s/cat
                       :record symbol?
                       :args (s/* ::pair)))

(s/def ::select-form vector?)
(s/def ::insert-form (s/or
                       :record ::record-form
                       :execute list?))
(s/def ::select-block (s/cat
                        :header #{:select}
                        :forms (s/* ::select-form)))
(s/def ::insert-block (s/cat
                        :header #{:insert}
                        :forms (s/* ::insert-form)))
(s/def ::rule (s/cat
                :select ::select-block
                :insert ::insert-block))
(s/def ::file (s/cat
                :init-forms (s/* ::insert-form)
                :rules (s/* ::rule)))

(defmacro init [nses files]
  (let [parsed-files (mapv #(s/conform ::file (read-file %))
                       files)
        init-forms (->> parsed-files
                        (mapcat :init-forms)
                        (mapv transform-insert-form))
        rules (->> parsed-files
                   (mapcat :rules)
                   vec)
        _ (add-rules rules)
        session (macros/sources-and-options->session-assembly-form
                  (map #(list 'quote %) nses))]
    `(do
       (pixel-midi-gogo.core/watch-files ~files)
       (reset! pixel-midi-gogo.core/*session
         (-> ~session ~@init-forms rules/fire-rules)))))

