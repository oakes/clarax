(ns play-cljc.state-test
  (:require [clojure.test :refer :all]
            [play-cljc.state :as state]
            [play-cljc.state.macros-java :refer [->state ->fact deffact]]))

(deffact Enemy [x y])

(deftest simple-query
  (-> (->state {:get-enemies [Enemy]})
      (state/insert! (->fact Enemy 0 0))
      (state/insert! (->fact Enemy 1 1))
      (state/insert! (->fact Enemy 2 2))
      (state/query :get-enemies)
      count
      (= 3)
      is))

