(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defmacro deffact [name fields & opts]
  (binding [build/*macro-name* (first &form)]
    (build/deffact* name fields opts)))

(defmacro ->state [& body]
  (binding [build/*rules* (atom {})
            build/*queries* (atom {})
            build/*facts* (atom #{})]
    (run! eval body)
    (let [{:keys [productions queries]} (build/get-state)]
      `{:session (compiler/mk-session ~productions)
        :queries ~queries})))

(defmacro ->query [& form]
  (binding [build/*macro-name* (first &form)]
    (build/->query* form)))

(defmacro ->rule [& form]
  (binding [build/*macro-name* (first &form)]
    (build/->rule* form)))

(defmacro ->fact [name & args]
  (build/->fact* name args))

