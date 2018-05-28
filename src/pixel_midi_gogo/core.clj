(ns pixel-midi-gogo.core
  (:require [clara.macros :as macros]
            [clara.rules :as rules]
            [cljs.env :as env]
            [clara.rules.dsl :as dsl]
            [clojure.java.io :as io]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :refer [indexing-push-back-reader]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def ^:const ns-sym 'pixel-midi-gogo.core)

(defn add-rule [name body]
  (let [rule (dsl/build-rule name body)]
    (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym name] rule)
    rule))

(defn add-query [name body]
  (let [query (dsl/build-query name body)]
    (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym name] query)
    query))

(defn transform-insert-form [{:keys [record args]}]
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

(defn transform-delete-form [form]
  (list 'pixel-midi-gogo.core/delete form))

(defn transform-update-form [{:keys [record args]}]
  (list
    'pixel-midi-gogo.core/edit
    record
    (->> args
         (map (juxt :key :val))
         (into {}))))

(defn transform-when-form [{:keys [binding record args]}]
  (let [query (vec (cons
                     record
                     (map (fn [{:keys [key val]}]
                            (let [key (symbol (name key))]
                              (if (list? val)
                                (walk/prewalk-replace {'% key} val)
                                (list '= key val))))
                       args)))]
    (if-let [{:keys [symbol arrow]} binding]
      (case arrow
        <<- [symbol '<- '(clara.rules.accumulators/distinct)
             :from query]
        <- (vec (concat [symbol '<-] query))
        nil)
      query)))

(defn transform-select-form [rule-sym {:keys [binding record]}]
  (list
    (:symbol binding)
    (list
      (list 'comp :?ret 'first)
      (list
        'some->
        '(deref pixel-midi-gogo.core/*session)
        (list
          'clara.rules/query
          (add-query
            (->> record name str/lower-case (format (str rule-sym "-%s-query")) symbol)
            (list [] ['?ret '<-
                      (case (:arrow binding)
                        <<- '(clara.rules.accumulators/distinct)
                        <- '(clara.rules.accumulators/max :timestamp :returns-fact true))
                      :from [record]])))))))

(defn add-rules [rules]
  (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym] {})
  (doseq [i (range (count rules))
          :let [{:keys [left right]} (get rules i)
                sym (symbol (str "rule-" i))
                left-side (mapv transform-when-form (:forms left))
                selects (->> right
                             (keep (fn [[type block]]
                                     (when (= type :select) block)))
                             (mapcat :forms))
                right-side (mapcat
                             (fn [[type block]]
                               (case type
                                 :select []
                                 :insert (map transform-insert-form (:forms block))
                                 :delete (map transform-delete-form (:forms block))
                                 :update (map transform-update-form (:forms block))
                                 :execute (:forms block)))
                             right)]]
    (add-rule sym (concat left-side ['=>]
                    (cond
                      (empty? right-side)
                      ['(do)]
                      (seq selects)
                      (list
                        (concat
                          (list 'let
                            (vec (mapcat (partial transform-select-form sym) selects)))
                          right-side))
                      :else
                      right-side)))))

(defn read-file [filename]
  (let [reader (indexing-push-back-reader (slurp (io/reader filename)))]
    (loop [forms []]
      (if-let [form (r/read {:eof nil} reader)]
        (recur (conj forms form))
        forms))))

(s/def ::binding (s/cat
                   :symbol symbol?
                   :arrow '#{<- <<-}))
(s/def ::pair (s/cat
                :key keyword?
                :val any?))

(s/def ::when-form (s/cat
                     :binding (s/? ::binding)
                     :record symbol?
                     :args (s/* ::pair)))
(s/def ::select-form (s/cat
                       :binding ::binding
                       :record symbol?))
(s/def ::insert-form (s/cat
                       :record symbol?
                       :args (s/* ::pair)))
(s/def ::delete-form #(not (keyword? %)))
(s/def ::update-form (s/cat
                       :record any?
                       :args (s/* ::pair)))
(s/def ::execute-form list?)

(s/def ::when-block (s/cat
                      :header #{:when}
                      :forms (s/+ (s/spec ::when-form))))
(s/def ::select-block (s/cat
                        :header #{:select}
                        :forms (s/+ (s/spec ::select-form))))
(s/def ::insert-block (s/cat
                        :header #{:insert}
                        :forms (s/+ (s/spec ::insert-form))))
(s/def ::delete-block (s/cat
                        :header #{:delete}
                        :forms (s/+ (s/spec ::delete-form))))
(s/def ::update-block (s/cat
                        :header #{:update}
                        :forms (s/+ (s/spec ::update-form))))
(s/def ::execute-block (s/cat
                         :header #{:execute}
                         :forms (s/+ (s/spec ::execute-form))))

(s/def ::rule (s/cat
                :left ::when-block
                :right (s/* (s/alt
                              :select ::select-block
                              :insert ::insert-block
                              :delete ::delete-block
                              :update ::update-block
                              :execute ::execute-block))))
(s/def ::file (s/cat
                :init-forms (s/* (s/spec ::insert-form))
                :rules (s/* ::rule)))

(defmacro init [nses files]
  (let [parsed-files (mapv #(s/conform ::file (read-file %)) files)
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

