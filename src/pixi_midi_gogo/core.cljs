(ns pixi-midi-gogo.core
  (:require [clara.rules :refer [query insert fire-rules]])
  (:require-macros [clara.rules :refer [defsession defrule defquery]]))

(defrecord Person [name email])

(enable-console-print!)

(defrule alice
  [Person (= "Alice" name) (= ?email email)]
  =>
  (js/console.log ?email))

(defquery get-all-people
  []
  [?person <- Person])

(defsession session 'pixi-midi-gogo.core)

(defn init []
  (-> session
      (insert (->Person "Alice" "alice@sekao.net"))
      (fire-rules)
      (query get-all-people)
      pr-str
      js/console.log))

(init)

