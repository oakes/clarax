(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defmacro deffact [name fields & opts]
  (binding [build/*macro-name* (first &form)]
    (build/deffact* name fields opts)))

(defmacro ->state [& body]
  (let [{:keys [productions queries]} (build/get-state body)]
    `{:session (compiler/mk-session ~productions)
      :queries ~queries}))

(defmacro ->fact [name & args]
  (build/->fact* name args))

