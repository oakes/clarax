(ns play-cljc.state-test
  (:require [clojure.test :refer :all]
            [play-cljc.state :as state]
            [play-cljc.state.macros-java :refer [->state ->fact deffact]]))

(deffact Enemy [x y])

(deftest query-latest
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

(deftest query-multiple
  (-> (->state {:get-enemies (fn []
                               (let [enemies [Enemy]]
                                 enemies))})
      (state/insert! (->fact Enemy 0 0))
      (state/insert! (->fact Enemy 1 1))
      (state/insert! (->fact Enemy 2 2))
      (state/query :get-enemies)
      count
      (= 3)
      is))

