(ns pixel-midi-gogo.app
  (:require [pixel-midi-gogo.core :refer [Fact insert]]
            [pixel-midi-gogo.browser :refer [Element ->Element Event]]
            [clara.rules :as rules]
            [clara.rules.accumulators :refer [all]])
  (:require-macros [pixel-midi-gogo.core :refer [read-rules]]))

(defrecord Person [name email])

(defrecord ListItem [text])

(read-rules
  [pixel-midi-gogo.core pixel-midi-gogo.browser]
  {:on [[?facts <- (all) :from [Fact (= ?id id) (some? id)]]]
   :do [(let [facts (sort-by :timestamp ?facts)
              current-fact (last facts)
              old-facts (butlast facts)]
          (doseq [{:keys [value] :as fact} old-facts]
            (rules/retract! fact)
            (when (and (record? value)
                       (not= value (:value current-fact)))
              (rules/retract! value))))]}
  {:on [[Person (= ?name name)]]
   :do [(insert nil (->ListItem (str "Hello, " ?name)))]}
  {:on [[Person (= ?email email)]
        [Fact (= id :stuff) (= ?value value)]]
   :do [(js/console.log ?email (pr-str ?value))]}
  {:do [(insert :stuff [:div "Hi"])]}
  {:do [(insert :contact (->Person "Alice" "alice@sekao.net"))]}
  {:do [(insert :contact (->Person "Bob" "bob@sekao.net"))]}
  {:on [[Event (= ?id id) (= ?type type)]]
   :do [(js/console.log "Event" (pr-str ?id) ?type)]}
  {:on [[?items <- (all) :from [ListItem]]]
   :do [(insert :root (->Element "#app"
                        [:div
                         [:input {:id :input
                                  :on-key-down true}]
                         [:button {:id :btn
                                   :on-click true}
                          "Click!"]
                         (into [:ul]
                           (for [item ?items]
                             [:li (:text item)]))]))]})

