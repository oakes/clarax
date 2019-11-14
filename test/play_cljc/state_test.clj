(ns play-cljc.state-test
  (:require [clojure.test :refer :all]
            [play-cljc.state :as state]
            [play-cljc.state.macros-java :refer [->state ->fact deffact]]))

(deffact Player [x y])
(deffact Enemy [x y])

(deftest query-enemy
  (-> (->state {:get-enemy (fn []
                             (let [enemy Enemy]
                               enemy))})
      (state/insert! (->fact Enemy 0 0))
      (state/insert! (->fact Enemy 1 1))
      (state/insert! (->fact Enemy 2 2))
      (state/query :get-enemy)
      :x
      (= 2)
      is))

(deftest query-enemies
  (-> (->state {:get-enemies (fn []
                               (let [enemy [Enemy]]
                                 enemy))})
      (state/insert! (->fact Enemy 0 0))
      (state/insert! (->fact Enemy 1 1))
      (state/insert! (->fact Enemy 2 2))
      (state/query :get-enemies)
      count
      (= 3)
      is))

(deftest query-multiple-facts
  (-> (->state {:get-entities (fn []
                                (let [enemy Enemy
                                      player Player]
                                  [enemy player]))})
      (state/insert! (->fact Enemy 0 0))
      (state/insert! (->fact Enemy 1 1))
      (state/insert! (->fact Enemy 2 2))
      (state/insert! (->fact Player 3 3))
      (state/query :get-entities)
      (as-> $ (let [[enemy player] $]
                (is (= (:x enemy) 2))
                (is (= (:x player) 3))))))

(deftest query-condition
  (-> (->state {:get-enemies (fn []
                               (let [enemy [Enemy]
                                     :when (> (:x enemy) 0)]
                                 enemy))})
      (state/insert! (->fact Enemy 0 0))
      (state/insert! (->fact Enemy 1 1))
      (state/insert! (->fact Enemy 2 2))
      (state/query :get-enemies)
      count
      (= 2)
      is))

