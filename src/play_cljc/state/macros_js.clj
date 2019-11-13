(ns play-cljc.state.macros-js
  (:require [play-cljc.state.build :as build]
            [clara.macros :as macros]
            [clara.rules :as rules]))

(defmacro deffact [name fields & opts]
  (binding [build/*macro-name* (first &form)
            *out* (java.io.StringWriter.)]
    (build/deffact* name fields opts)))

(defmacro ->state [& body]
  (binding [build/*rules* (atom {})
            build/*queries* (atom {})
            build/*facts* (atom #{})]
    (run! eval body)
    (let [{:keys [productions queries]} (build/get-state)]
      {:session (-> productions
                    eval
                    (macros/productions->session-assembly-form []))
       :queries queries})))

(defmacro ->query [& form]
  (binding [build/*macro-name* (first &form)
            *out* (java.io.StringWriter.)]
    (build/->query* form)))

(defmacro ->rule [& form]
  (binding [build/*macro-name* (first &form)
            *out* (java.io.StringWriter.)]
    (build/->rule* form)))

(defmacro ->fact [name & args]
  (build/->fact* name args))

