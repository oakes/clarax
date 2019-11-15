(ns play-cljc.state-test
  (:require [clojure.test :refer :all]
            [play-cljc.state :as state]
            [play-cljc.state.macros-java :refer [->state]]
            [clara.rules :as clara]))

(defrecord Player [x y hp])
(defrecord Enemy [x y hp])

(deftest query-enemy
  (-> (->state {:get-enemy
                (fn []
                  (let [enemy Enemy]
                    enemy))})
      (clara/insert (->Enemy 2 2 10))
      (clara/query :get-enemy)
      :x
      (= 2)
      is))

(deftest query-enemies
  (-> (->state {:get-enemies
                (fn []
                  (let [enemy [Enemy]]
                    enemy))})
      (clara/insert (->Enemy 0 0 10))
      (clara/insert (->Enemy 1 1 10))
      (clara/insert (->Enemy 2 2 10))
      (clara/query :get-enemies)
      count
      (= 3)
      is))

(deftest query-multiple-facts
  (as-> (->state {:get-enemy
                  (fn []
                    (let [enemy Enemy]
                      enemy))
                  :get-entities
                  (fn []
                    (let [enemy Enemy
                          player Player]
                      [enemy player]))})
        $
        (clara/insert $ (->Enemy 0 0 10))
        (state/merge $ (clara/query $ :get-enemy) {:x 1 :y 1})
        (state/merge $ (clara/query $ :get-enemy) {:x 2 :y 2})
        (clara/insert $ (->Player 3 3 10))
        (clara/query $ :get-entities)
        (let [[enemy player] $]
          (is (= (:x enemy) 2))
          (is (= (:x player) 3)))))

(deftest query-condition
  (-> (->state {:get-enemies
                (fn []
                  (let [enemy [Enemy]
                        :when (>= (:x enemy) 1)]
                    enemy))})
      (clara/insert (->Enemy 0 0 10))
      (clara/insert (->Enemy 1 1 10))
      (clara/insert (->Enemy 2 2 10))
      (clara/query :get-enemies)
      count
      (= 2)
      is))

(deftest query-parameter
  (-> (->state {:get-enemy
                (fn [?x ?y]
                  (let [player Player
                        enemy Enemy
                        :when (and (= (:x enemy) ?x)
                                   (= (:y enemy) ?y))]
                    enemy))})
      (clara/insert (->Enemy 1 0 10))
      (clara/insert (->Player 3 3 10))
      (clara/query :get-enemy :?x 1 :?y 0)
      record?
      is))

(deftest query-and-rule
  (as-> (->state {:get-player
                  (fn []
                    (let [player Player]
                      player))
                  :get-enemy
                  (fn []
                    (let [enemy Enemy]
                      enemy))
                  :hurt-enemy
                  (let [player Player
                        enemy Enemy
                        :when (and (= (:x player) (:x enemy))
                                   (= (:y player) (:y enemy)))]
                    (state/merge! player {:x (inc (:x player))})
                    (state/merge! enemy {:hp (dec (:hp enemy))}))})
        $
        (clara/insert $ (->Enemy 0 0 10))
        (clara/insert $ (->Player 3 3 10))
        (state/merge $ (clara/query $ :get-player) {:x 0 :y 0})
        (state/merge $ (clara/query $ :get-player) {:x 0 :y 10})
        (let [{:keys [hp]} (clara/query $ :get-enemy)]
          (is (= hp 9)))))

