(ns pixi-midi-gogo.core
  (:require [clara.rules :refer [insert fire-rules]])
  (:require-macros [clara.rules :refer [defsession defrule]]))

(defrecord Person [name email])

(defsession session 'pixi-midi-gogo.core)

(enable-console-print!)

(defrule alice
  [Person (= "Alice" name) (= ?email email)]
  =>
  (js/console.log ?email))

(-> session
    (insert (->Person "Alice" "alice@sekao.net"))
    (fire-rules))

