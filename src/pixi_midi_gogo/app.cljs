(ns pixi-midi-gogo.app
  (:require [pixi-midi-gogo.core :refer [Person ->Person insert]]
            [pixi-midi-gogo.browser :refer [->Element]]
            [clara.rules :as rules])
  (:require-macros [pixi-midi-gogo.core :refer [read-rules]]))

(rules/fire-rules
  (read-rules
    {:in pixi-midi-gogo.browser
     :on [?person <- Person]
     :do (insert (->Element [:h1 (str "Hello, " (:name ?person))] "#app2"))}
    {:on [Person (= "Alice" name) (= ?email email)]
     :do (js/console.log ?email)}
    {:do (insert (->Person "Alice" "alice@sekao.net"))}
    {:do (insert (->Person "Bob" "bob@sekao.net"))}
    {:in pixi-midi-gogo.browser
     :do (insert (->Element [:h1 "Hello, world!"] "#app"))}))

