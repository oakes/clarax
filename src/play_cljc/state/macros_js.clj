(ns play-cljc.state.macros-js
  (:require [play-cljc.state.build :as build]
            [clara.macros :as macros]
            [clara.rules :as rules]))

(defmacro ->state [rules-and-queries]
  (let [{:keys [productions queries query-fns]} (build/get-state rules-and-queries)]
    `(play-cljc.state.Session. ~(-> productions
                                    eval
                                    (macros/productions->session-assembly-form []))
                               ~queries
                               ~query-fns)))

