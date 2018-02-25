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

(def default-rules
  '[])

(defn add-rule [name body]
  (->> (dsl/build-rule name body)
       (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym name])))

(defn add-rules [rules]
  (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym] {})
  (doseq [i (range (count rules))
          :let [{:keys [select right-side]} (get rules i)
                sym (symbol (str "rule-" i))
                {:keys [insert execute]} (into {} right-side)
                right-side (->> (:forms insert)
                                (map #(list 'pixel-midi-gogo.core/insert %))
                                (concat (:forms execute)))]]
    (add-rule sym (concat (:forms select) ['=>]
                    (if (empty? right-side) ['(do)] right-side)))))

(defn read-file [filename]
  (let [reader (indexing-push-back-reader (slurp (io/reader filename)))]
    (loop [forms []]
      (if-let [form (r/read {:eof nil} reader)]
        (recur (conj forms form))
        forms))))

(s/def ::callable-form list?)
(s/def ::query-form vector?)
(s/def ::select-block (s/cat
                        :header #{:select}
                        :forms (s/* ::query-form)))
(s/def ::insert-block (s/cat
                        :header #{:insert}
                        :forms (s/* ::callable-form)))
(s/def ::execute-block (s/cat
                         :header #{:execute}
                         :forms (s/* ::callable-form)))
(s/def ::rule (s/cat
                :select ::select-block
                :right-side (s/* (s/alt
                                   :insert ::insert-block
                                   :execute ::execute-block))))
(s/def ::file (s/cat
                :init-forms (s/* ::callable-form)
                :rules (s/* ::rule)))

(defmacro init [nses files]
  (let [parsed-files (mapv #(s/conform ::file
                              (-> % read-file (concat default-rules)))
                       files)
        init-forms (->> parsed-files
                        (mapcat :init-forms)
                        (mapv #(list 'pixel-midi-gogo.core/insert %)))
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

