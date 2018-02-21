(ns pixel-midi-gogo.app
  (:require [pixel-midi-gogo.core :refer [Def ->Def]]
            [pixel-midi-gogo.element :refer [Element ->Element]]
            [pixel-midi-gogo.event :refer [Event]]
            [clara.rules :as rules]
            [clara.rules.accumulators :refer [all]])
  (:require-macros [pixel-midi-gogo.core :refer [read-rules]]))

(defrecord Person [name email])

(defrecord ListItem [text])

(read-rules
  [pixel-midi-gogo.core pixel-midi-gogo.element pixel-midi-gogo.event]
  
  (->Def :stuff [:div "Hi"])
  (->Def :contact (->Person "Alice" "alice@sekao.net"))
  (->Def :contact (->Person "Bob" "bob@sekao.net"))
  
  :select
  [Person (= ?name name)]
  :insert
  (->ListItem (str "Hello, " ?name))
  
  :select
  [Person (= ?email email)]
  [Def (= id :stuff) (= ?value value)]
  :execute
  (js/console.log ?email (pr-str ?value))
  
  :select
  [Event (= ?id id) (= ?type type)]
  :execute
  (js/console.log "Event" (pr-str ?id) ?type)
  
  :select
  [?items <- (all) :from [ListItem]]
  :insert
  (->Def :root
    (->Element "#app"
               [:div
                [:input {:id :input
                         :on-key-down true}]
                [:button {:id :btn
                          :on-click true}
                 "Click!"]
                (into [:ul]
                  (for [item ?items]
                    [:li (:text item)]))])))

