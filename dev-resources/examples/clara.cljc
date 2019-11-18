(ns examples.clara
  (:require [clara.rules :as clara #?(:clj :refer :cljs :refer-macros) [defsession defquery defrule]]
            [clara.rules.accumulators :as acc]))

(defrecord Player [x y health])
(defrecord Enemy [x y health])

(defrule remove-dead-enemies
  [?enemy <- Enemy (= health 0)]
  =>
  (clara/retract! ?enemy))

(defquery get-player
  []
  [?player <- Player])

(defquery get-nearby-enemies
  []
  [?player <- Player]
  [?enemy <- (acc/all) :from [Enemy (= (:x ?player) x) (= (:y ?player) y)]])

(defquery get-enemies-at
  [:?x :?y]
  [?enemy <- (acc/all) :from [Enemy (= ?x x) (= ?y y)]])

(defsession session 'examples.clara)

(def *session
  (-> session
      (clara/insert (->Player 1 1 10)
                    (->Enemy 1 1 10)
                    (->Enemy 1 1 10)
                    (->Enemy 2 2 10)
                    (->Enemy 2 2 0))
      clara/fire-rules
      atom))

(-> (clara/query @*session get-player)
    first :?player println)
;; => #examples.clara.Player{:x 1, :y 1, :health 10}

(-> (clara/query @*session get-nearby-enemies)
    first :?enemy println)
;; => [#examples.clara.Enemy{:x 1, :y 1, :health 10} #examples.clara.Enemy{:x 1, :y 1, :health 10}]

(-> (clara/query @*session get-enemies-at :?x 2 :?y 2)
    first :?enemy println)
;; => [#examples.clara.Enemy{:x 2, :y 2, :health 10}]
;; this only returns one, because the other enemy at (2, 2) was removed by the remove-dead-enemies rule

