(ns clarax.macros-js
  (:require [clarax.parse :as parse]
            [clara.macros :as macros]
            [clara.rules :as rules]))

(defmacro ->session [rules-and-queries]
  (let [{:keys [productions queries query-fns]} (parse/->productions rules-and-queries)]
    `(clarax.rules.Session. ~(-> productions
                                 eval
                                 (macros/productions->session-assembly-form []))
                            ~queries
                            ~query-fns)))

