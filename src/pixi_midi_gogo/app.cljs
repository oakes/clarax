(ns pixi-midi-gogo.app
  (:require [pixi-midi-gogo.core :refer [Fact insert]]
            [pixi-midi-gogo.browser :refer [Element ->Element]]
            [clara.rules :as rules]
            [clara.rules.accumulators :refer [all]]
            [clara.tools.inspect :refer [explain-activations]])
  (:require-macros [pixi-midi-gogo.core :refer [read-rules]]))

(defrecord Person [name email])

(defrecord ListItem [text])

(rules/fire-rules
  (read-rules
    [pixi-midi-gogo.core pixi-midi-gogo.browser]
    {:on [[?facts <- (all) :from [Fact (= ?id id) (some? id)]]]
     :do [(doseq [fact (butlast (sort-by :timestamp ?facts))]
            (rules/retract! fact)
            (rules/retract! (:record fact)))]}
    {:on [[Person (= ?name name)]]
     :do [(insert nil (->ListItem (str "Hello, " ?name)))]}
    {:on [[Person (= ?email email)]]
     :do [(js/console.log ?email)]}
    {:do [(insert :contact (->Person "Alice" "alice@sekao.net"))]}
    {:do [(insert :contact (->Person "Bob" "bob@sekao.net"))]}
    {:on [[?items <- (all) :from [ListItem]]]
     :do [(insert nil (->Element "#app"
                        (into [:ul]
                          (for [item ?items]
                            [:li (:text item)]))))]}))

