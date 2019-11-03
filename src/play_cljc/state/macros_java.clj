(ns play-cljc.state.macros-java
  (:require [play-cljc.state.build :as build]
            [clara.rules.compiler :as compiler]
            [clara.rules :as rules]))

(defmacro init-state [*session & forms]
  (let [{:keys [init-forms rules]} (build/forms->rules *session forms)]
    `(->> ~rules compiler/mk-session ~@init-forms rules/fire-rules (reset! ~*session))))

