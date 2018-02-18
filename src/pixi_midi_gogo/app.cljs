(ns pixi-midi-gogo.app
  (:require [pixi-midi-gogo.core :refer [insert]]
            [pixi-midi-gogo.browser :refer [Element ->Element]]
            [clara.rules :as rules]
            [clara.rules.accumulators :refer [all]])
  (:require-macros [pixi-midi-gogo.core :refer [read-rules]]))

(defrecord Person [name email])

(defrecord ListItem [text])

(rules/fire-rules
  (read-rules
    [pixi-midi-gogo.core pixi-midi-gogo.browser]
    {:on [[Person (= ?name name)]]
     :do [(insert (->ListItem (str "Hello, " ?name)))]}
    {:on [[Person (= ?email email)]]
     :do [(js/console.log ?email)]}
    {:do [(insert (->Person "Alice" "alice@sekao.net"))]}
    {:do [(insert (->Person "Bob" "bob@sekao.net"))]}
    {:on [[?items <- (all) :from [ListItem]]]
     :do [(insert (->Element "#app"
                    (into [:ul]
                      (for [item ?items]
                        [:li (:text item)]))))]}))

