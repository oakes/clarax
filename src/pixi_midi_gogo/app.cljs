(ns pixi-midi-gogo.app
  (:require [pixi-midi-gogo.core :refer [Fact insert]]
            [pixi-midi-gogo.browser :refer [Element ->Element Event]]
            [clara.rules :as rules]
            [clara.rules.accumulators :refer [all]]
            [clara.tools.inspect :refer [explain-activations]])
  (:require-macros [pixi-midi-gogo.core :refer [read-rules]]))

(defrecord Person [name email])

(defrecord ListItem [text])

(read-rules
  [pixi-midi-gogo.core pixi-midi-gogo.browser]
  {:on [[?facts <- (all) :from [Fact (= ?id id) (some? id)]]]
   :do [(doseq [{:keys [value] :as fact} (butlast (sort-by :timestamp ?facts))]
          (rules/retract! fact)
          (when (record? value)
            (rules/retract! value)))]}
  {:on [[Person (= ?name name)]]
   :do [(insert nil (->ListItem (str "Hello, " ?name)))]}
  {:on [[Person (= ?email email)]
        [Fact (= id :stuff) (= ?value value)]]
   :do [(js/console.log ?email (pr-str ?value))]}
  {:do [(insert :stuff [:div "Hi"])]}
  {:do [(insert :contact (->Person "Alice" "alice@sekao.net"))]}
  {:do [(insert :contact (->Person "Bob" "bob@sekao.net"))]}
  {:on [[Event (= type "click") (= ?id id)]]
   :do [(js/console.log "Clicked" (pr-str ?id))]}
  {:on [[?items <- (all) :from [ListItem]]]
   :do [(insert :root (->Element "#app"
                        [:div
                         [:button {:id :btn}
                          "Click!"]
                         (into [:ul]
                           (for [item ?items]
                             [:li (:text item)]))]))]})

