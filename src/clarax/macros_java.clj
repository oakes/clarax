(ns clarax.macros-java
  (:require [clarax.parse :as parse]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defmacro ->state [rules-and-queries]
  (let [{:keys [productions queries query-fns]} (parse/get-state rules-and-queries)]
    `(clarax.rules.Session. (compiler/mk-session ~productions)
                            ~queries
                            ~query-fns)))

