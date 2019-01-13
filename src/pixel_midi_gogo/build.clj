(ns pixel-midi-gogo.build
  (:require [pixel-midi-gogo.core]
            [clara.rules.compiler :as compiler]
            [clara.macros :as macros]
            [clara.rules :as rules]
            [cljs.env :as env]
            [clara.rules.dsl :as dsl]
            [clojure.java.io :as io]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :refer [indexing-push-back-reader]]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]))

(def ^:const ns-sym 'pixel-midi-gogo.core)

(def ^:dynamic *cljs?* false)

(def default-rules
  '[:when
    [?canvas <- Canvas]
    :execute
    (@pixel-midi-gogo.core/*send-action-fn "canvas-insert" ?canvas)])

(defn add-rule [name body]
  (let [rule (dsl/build-rule name body)]
    (when *cljs?*
      (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym name] rule))
    rule))

(defn add-query [name body]
  (let [query (dsl/build-query name body)]
    (when *cljs?*
      (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym name] query))
    query))

(defn transform-args [args]
  (->> args
       (map (juxt :key :val))
       (into {})))

(defn transform-insert-form [{:keys [record args]}]
  (list
    'pixel-midi-gogo.core/insert
    (list
      (symbol (str "map->" record))
      (update (transform-args args)
        :timestamp
        #(or % '(pixel-midi-gogo.core/get-time))))))

(defn transform-delete-form [form]
  (list 'pixel-midi-gogo.core/delete form))

(defn transform-update-form [{:keys [record args]}]
  (list
    'pixel-midi-gogo.core/edit
    record
    (transform-args args)))

(defn transform-upsert-form [query {:keys [record args update-args]}]
  (let [new-args (update (transform-args (concat args update-args))
                   :timestamp
                   #(or % '(pixel-midi-gogo.core/get-time)))]
    (list
      'pixel-midi-gogo.core/upsert
      query
      (list
        (symbol (str "map->" record))
        new-args)
      new-args)))

(defn build-query [{:keys [record args]}]
  (vec (cons
         record
         (map (fn [{:keys [key val]}]
                (let [key (symbol (name key))]
                  (if (list? val)
                    (walk/prewalk-replace {'% key} val)
                    (list '= key val))))
           args))))

(defn transform-when-form [{:keys [binding] :as form}]
  (let [query (build-query form)]
    (if-let [{:keys [symbol arrow]} binding]
      (case arrow
        <<- [symbol '<- '(clara.rules.accumulators/distinct)
             :from query]
        <- (vec (concat [symbol '<-] query)))
      query)))

(defn select-form->query [query-name {:keys [binding] :as form}]
  (add-query query-name
    (list [] ['?ret '<-
              (case (:arrow binding)
                <<- '(clara.rules.accumulators/distinct)
                <- '(clara.rules.accumulators/max :timestamp :returns-fact true))
              :from (build-query form)])))

(defn transform-select-form [query {:keys [binding] :as form}]
  (list
    (:symbol binding)
    (list
      (list 'comp :?ret 'first)
      (list
        'some->
        '(deref pixel-midi-gogo.core/*session)
        (list 'clara.rules/query query)))))

(defn add-rules [rules]
  (when *cljs?*
    (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym] {}))
  (flatten
    (for [i (range (count rules))
          :let [{:keys [left right]} (get rules i)
                sym (symbol (str "rule-" i))
                left-side (mapv transform-when-form (:forms left))
                selects (->> right
                             (keep (fn [[type block]]
                                     (when (= type :select) block)))
                             (mapcat :forms)
                             vec)
                upserts (->> right
                             (keep (fn [[type block]]
                                     (when (= type :upsert) block)))
                             (mapcat :forms)
                             vec)
                queries (reduce-kv
                          (fn [v i form]
                            (conj v (select-form->query (str sym "-query-" i) form)))
                          []
                          (into selects upserts))
                [select-queries upsert-queries] (split-at (count selects) queries)
                right-side (mapcat
                             (fn [[type block]]
                               (case type
                                 :select []
                                 :insert (map transform-insert-form (:forms block))
                                 :delete (map transform-delete-form (:forms block))
                                 :update (map transform-update-form (:forms block))
                                 :upsert (map transform-upsert-form upsert-queries (:forms block))
                                 :execute (:forms block)))
                             right)]]
      (cons (add-rule sym (concat left-side ['=>]
                            (cond
                              (empty? right-side)
                              ['(do)]
                              (seq selects)
                              (list
                                (concat
                                  (list 'let (vec (mapcat transform-select-form select-queries selects)))
                                  right-side))
                              :else
                              right-side)))
        queries))))

(defn read-file [filename]
  (let [reader (indexing-push-back-reader (slurp (io/reader filename)))]
    (loop [forms []]
      (if-let [form (r/read {:eof nil} reader)]
        (recur (conj forms form))
        (into forms default-rules)))))

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
                       :record symbol?
                       :args (s/* ::pair)))
(s/def ::insert-form (s/cat
                       :record symbol?
                       :args (s/* ::pair)))
(s/def ::delete-form #(not (keyword? %)))
(s/def ::update-form (s/cat
                       :record any?
                       :args (s/* ::pair)))
(s/def ::upsert-form (s/cat
                       :record symbol?
                       :args (s/* ::pair)
                       :binding (s/cat :arrow '#{<-})
                       :update-args (s/* ::pair)))
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
(s/def ::upsert-block (s/cat
                        :header #{:upsert}
                        :forms (s/+ (s/spec ::upsert-form))))
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
                              :upsert ::upsert-block
                              :execute ::execute-block))))
(s/def ::file (s/cat
                :init-forms (s/* (s/spec ::insert-form))
                :rules (s/* ::rule)))

(defmacro init-cljs [nses files]
  (let [parsed-files (mapv #(s/conform ::file (read-file %)) files)
        init-forms (->> parsed-files
                        (mapcat :init-forms)
                        (mapv transform-insert-form))
        rules (->> parsed-files
                   (mapcat :rules)
                   vec)
        _ (binding [*cljs?* true]
            (add-rules rules))
        session (macros/sources-and-options->session-assembly-form
                  (map #(list 'quote %) nses))]
    `(do
       (pixel-midi-gogo.core/watch-files ~files)
       (reset! pixel-midi-gogo.core/*session
         (-> ~session ~@init-forms rules/fire-rules)))))

(extend-type java.util.Map
  compiler/IRuleSource
  (load-rules [m]
    [m]))

(defn init-clj [ns files]
  (let [parsed-files (mapv #(s/conform ::file (read-file %)) files)
        init-forms (->> parsed-files
                        (mapcat :init-forms)
                        (mapv transform-insert-form))
        rules (binding [*ns* ns]
                (->> parsed-files
                     (mapcat :rules)
                     vec
                     add-rules
                     (mapv eval)))
        session (compiler/mk-session rules)
        init-session (binding [*ns* ns]
                       (eval (list 'fn '[session] (concat '[-> session] init-forms))))]
    (reset! pixel-midi-gogo.core/*session
      (-> session init-session rules/fire-rules))))

