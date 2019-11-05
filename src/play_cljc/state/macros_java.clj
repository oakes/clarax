(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defn ->session [& rules-and-queries]
  (compiler/mk-session rules-and-queries))

(defmacro defquery [& form]
  `(def ~(first form) ~(build/form->query form)))

(defmacro defrule [& form]
  `(def ~(first form) ~(build/form->rule form)))

