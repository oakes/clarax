(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defmacro deffact [name fields & opts]
  (build/deffact* name fields opts))

(defmacro ->state [rules-and-queries]
  (let [{:keys [productions queries query-fns]} (build/get-state rules-and-queries)]
    `{:session (compiler/mk-session ~productions)
      :queries ~queries
      :query-fns ~query-fns}))

(defmacro ->fact [name & args]
  (build/->fact* name args))

