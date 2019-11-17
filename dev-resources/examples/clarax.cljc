(ns examples.clarax
  (:require [clara.rules :as clara]
            [clara.rules.accumulators :as acc]
            #?(:clj  [clarax.macros-java :refer [->session]]
               :cljs [clarax.macros-js :refer-macros [->session]])))

(defrecord Player [x y health])
(defrecord Enemy [x y health])

(def *session
  (-> ;; all rules/queries are specified in a single hash map.
      ;; as you can see, rules look like familiar `let` expressions,
      ;; mapping a name (enemy) to a record type (Enemy).
      ;; any binding pair can be followed by a :when expression,
      ;; similar to the :when expressions in `for`, `doseq`, etc.
      {:remove-dead-enemies
       (let [enemy Enemy
             :when (= (:health enemy) 0)]
         ;; the body of the `let` is what will run when the rule fires
         (clara/retract! enemy))

       ;; queries look exactly like rules, except the are surrounded by `fn`
       :get-player
       (fn []
         (let [player Player]
           ;; the body of the `let` determines what the query will return.
           ;; in this case, it will simply return the entire player record.
           player))

       :get-nearby-enemies
       (fn []
         (let [player Player
               enemy [Enemy (acc/all)] ;; this is how you use an accumulator
               :when (and (= (:x player) (:x enemy))
                          (= (:y player) (:y enemy)))]
           enemy))

       :get-enemies-at
       (fn [?x ?y]
         (let [{:keys [x y] :as enemy} [Enemy (acc/all)] ;; you can destructure just like in a normal `let` form
               :when (and (= ?x x) (= ?y y))]
           enemy))}
      ;; this macro creates the session from the hash map
      ->session
      ;; you use the same functions from clara to insert and fire rules
      (clara/insert (->Player 1 1 10)
                    (->Enemy 1 1 10)
                    (->Enemy 1 1 10)
                    (->Enemy 2 2 10)
                    (->Enemy 2 2 0))
      clara/fire-rules
      atom))

(println (clara/query @*session :get-player))
;; => #examples.clara.Player{:x 1, :y 1, :health 10}

(println (clara/query @*session :get-nearby-enemies))
;; => [#examples.clara.Enemy{:x 1, :y 1, :health 10} #examples.clara.Enemy{:x 1, :y 1, :health 10}]

(println (clara/query @*session :get-enemies-at :?x 2 :?y 2))
;; => [#examples.clara.Enemy{:x 2, :y 2, :health 10}]
;; this only returns one, because the other enemy at (2, 2) was removed by the remove-dead-enemies rule

