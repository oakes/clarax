(ns pixi-midi-gogo.core
  (:require [clara.rules :refer [query insert fire-rules]])
  (:require-macros [clara.rules :refer [defsession defrule defquery]]
                   [pixi-midi-gogo.core :refer [read-rules]]))

(defrecord Person [name email])

(def session
  (read-rules
    {:in pixi-midi-gogo.core
     :on [?person <- Person]
     :do (js/console.log (pr-str ?person))}
    {:in pixi-midi-gogo.core
     :on [Person (= "Alice" name) (= ?email email)]
     :do (js/console.log ?email)}))

(defn init []
  (-> session
      (insert (->Person "Alice" "alice@sekao.net"))
      (insert (->Person "Bob" "bob@sekao.net"))
      (fire-rules)))

(init)

