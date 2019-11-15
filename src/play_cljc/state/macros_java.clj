(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defmacro ->state [rules-and-queries]
  (let [{:keys [productions queries query-fns]} (build/get-state rules-and-queries)]
    `(play_cljc.state.Session. (compiler/mk-session ~productions)
                               ~queries
                               ~query-fns)))

