(ns clarax.rules-test
  (:require [clojure.test :refer :all]
            [clarax.macros-java :refer [->session]]
            [clarax.rules :as clarax]
            [clara.rules :as clara]))

(defrecord Player [x y hp])
(defrecord Enemy [x y hp])

(deftest query-enemy
  (-> (->session {:get-enemy
                  (fn []
                    (let [enemy Enemy]
                      enemy))})
      (clara/insert (->Enemy 2 2 10))
      clara/fire-rules
      (clara/query :get-enemy)
      :x
      (= 2)
      is))

(deftest query-enemies
  (-> (->session {:get-enemies
                  (fn []
                    (let [enemy [Enemy]]
                      enemy))})
      (clara/insert (->Enemy 0 0 10))
      (clara/insert (->Enemy 1 1 10))
      (clara/insert (->Enemy 2 2 10))
      clara/fire-rules
      (clara/query :get-enemies)
      count
      (= 3)
      is))

(deftest query-multiple-facts
  (as-> (->session {:get-enemy
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
        (clara/fire-rules $)
        (clarax/merge $ (clara/query $ :get-enemy) {:x 1 :y 1})
        (clarax/merge $ (clara/query $ :get-enemy) {:x 2 :y 2})
        (clara/insert $ (->Player 3 3 10))
        (clara/fire-rules $)
        (clara/query $ :get-entities)
        (let [[enemy player] $]
          (is (= (:x enemy) 2))
          (is (= (:x player) 3)))))

(deftest query-condition
  (-> (->session {:get-enemies
                  (fn []
                    (let [{:keys [x] :as enemy} [Enemy]
                          :when (>= x 1)]
                      enemy))})
      (clara/insert (->Enemy 0 0 10))
      (clara/insert (->Enemy 1 1 10))
      (clara/insert (->Enemy 2 2 10))
      clara/fire-rules
      (clara/query :get-enemies)
      count
      (= 2)
      is))

(deftest query-parameter
  (-> (->session {:get-enemy
                  (fn [?x ?y]
                    (let [player Player
                          {:keys [x y] :as enemy} Enemy
                          :when (and (= x ?x) (= y ?y))]
                      enemy))})
      (clara/insert (->Enemy 1 0 10))
      (clara/insert (->Player 3 3 10))
      clara/fire-rules
      (clara/query :get-enemy :?x 1 :?y 0)
      record?
      is))

(deftest query-and-rule
  (as-> (->session {:get-player
                    (fn []
                      (let [player Player]
                        player))
                    :get-enemy
                    (fn []
                      (let [enemy Enemy]
                        enemy))
                    :hurt-enemy
                    (let [player Player
                          {:keys [x y] :as enemy} Enemy
                          :when (and (= (:x player) x)
                                     (= (:y player) y))]
                      (clarax/merge! player {:x (inc (:x player))})
                      (clarax/merge! enemy {:hp (dec (:hp enemy))}))})
        $
        (clara/insert $ (->Enemy 0 0 10))
        (clara/insert $ (->Player 3 3 10))
        (clara/fire-rules $)
        (clarax/merge $ (clara/query $ :get-player) {:x 0 :y 0})
        (clarax/merge $ (clara/query $ :get-player) {:x 0 :y 10})
        (clara/fire-rules $)
        (let [{:keys [hp]} (clara/query $ :get-enemy)]
          (is (= hp 9)))))

(def player-queries
  '{:get-player
    (fn []
      (let [player Player]
        player))
    :get-players
    (fn []
      (let [player [Player]]
        player))})

(def enemy-queries
  '{:get-enemy
    (fn []
      (let [enemy Enemy]
        enemy))
    :get-enemies
    (fn []
      (let [enemy [Enemy]]
        enemy))})

(defmacro ->session* []
  `(->session ~(merge player-queries enemy-queries)))

(deftest query-with-macro
  (-> (->session*)
      (clara/insert (->Enemy 0 0 10))
      (clara/insert (->Player 3 3 10))
      clara/fire-rules
      (clara/query :get-player)
      :x
      (= 3)
      is))

(deftest rule-with-join
  (let [session (->session
                  {:overlapping
                   (let [player Player
                         :when (= ?x (:x player))
                         enemy Enemy
                         :when (= ?x (:x enemy))]
                     (clarax/merge! player (-> player
                                               (update :x inc)
                                               (update :hp - 2))))
                   :get-player
                   (fn []
                     (let [player Player]
                       player))})]
    (testing "they are overlapping, so the player's health should decrease"
      (-> session
          (clara/insert (->Enemy 1 1 10) (->Player 1 1 10))
          clara/fire-rules
          (clara/query :get-player)
          :hp
          (= 8)
          is))
    (testing "they are not overlapping, so the player's health should not change"
      (-> session
          (clara/insert (->Enemy 0 1 10) (->Player 1 1 10))
          clara/fire-rules
          (clara/query :get-player)
          :hp
          (= 10)
          is))))

