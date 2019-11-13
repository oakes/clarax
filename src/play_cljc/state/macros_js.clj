(ns play-cljc.state.macros-js
  (:require [play-cljc.state.build :as build]
            [clara.macros :as macros]
            [clara.rules :as rules]))

(defmacro deffact [name fields & opts]
  (binding [build/*macro-name* (first &form)
            *out* (java.io.StringWriter.)]
    (build/deffact* name fields opts)))

(defmacro ->state [& body]
  (let [{:keys [productions queries]} (build/get-state body)]
    {:session (-> productions
                  eval
                  (macros/productions->session-assembly-form []))
     :queries queries}))

(defmacro ->fact [name & args]
  (build/->fact* name args))

