(ns pixi-midi-gogo.app
  (:require [pixi-midi-gogo.core :refer [Person ->Person insert]]
            [pixi-midi-gogo.browser :refer [Element ->Element]]
            [clara.rules :as rules])
  (:require-macros [pixi-midi-gogo.core :refer [read-rules]]))

(rules/fire-rules
  (read-rules
    [pixi-midi-gogo.core pixi-midi-gogo.browser]
    {:on [[Person (= ?name name)]]
     :do [(insert (->Element :greeting [:h1 (str "Hello, " ?name)]))]}
    {:on [[Person (= ?email email)]]
     :do [(js/console.log ?email)]}
    {:do [(insert (->Person nil "Alice" "alice@sekao.net"))]}
    {:do [(insert (->Person nil "Bob" "bob@sekao.net"))]}
    {:on [[Element (= :greeting id) (= ?value value)]]
     :do [(insert (->Element "#app" [:div "Hello, world!" ?value]))]}))

