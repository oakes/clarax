(ns play-cljc.gl.examples-state
  (:require [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.gl.example-utils :as eu]
            [play-cljc.gl.example-data :as data]
            [play-cljc.transforms :as t]
            [play-cljc.instances :as i]
            [play-cljc.primitives-2d :as primitives]
            [play-cljc.state :as state]
            #?(:clj  [play-cljc.macros-java :refer [gl]]
               :cljs [play-cljc.macros-js :refer-macros [gl]])
            #?(:clj [dynadoc.example :refer [defexample]])
            #?(:clj  [play-cljc.state.macros-java :refer [->session defquery defrule]]
               :cljs [play-cljc.state.macros-js :refer-macros [->session defquery defrule]]))
  #?(:cljs (:require-macros [dynadoc.example :refer [defexample]])))

(defrecord Stuff [x y])

(defrecord Thing [])

(def *state (atom nil))

(defquery get-stuff <- Stuff)

(defrule new-thing
  [?thing <- Thing]
  =>
  (println "stuff:" (state/query @*state get-stuff)))

(defrule new-stuff
  [?stuff <- Stuff]
  =>
  (println ?stuff)
  (state/insert! (->Thing)))

(reset! *state
    (-> (->session get-stuff new-thing new-stuff)
        (state/insert! (->Stuff 0 0))))

(swap! *state state/insert! (->Stuff 3 4))

(println (state/query @*state get-stuff))

;; rect

(defn rect-example [game entity]
  (gl game disable (gl game CULL_FACE))
  (gl game disable (gl game DEPTH_TEST))
  (->> entity
       (c/compile game)
       (assoc game :entity)))

(defexample play-cljc.state/rect-example
  {:with-card card
   :with-focus [focus (-> entity
                          (play-cljc.transforms/color [1 0 0 1])
                          (play-cljc.transforms/translate 50 50)
                          (play-cljc.transforms/scale 100 100))]}
  (let [game (play-cljc.gl.example-utils/init-example card)
        game-width (play-cljc.gl.example-utils/get-width game)
        game-height (play-cljc.gl.example-utils/get-height game)
        entity (-> (play-cljc.gl.entities-2d/->entity game play-cljc.primitives-2d/rect)
                   (assoc :viewport {:x 0 :y 0 :width game-width :height game-height})
                   (play-cljc.transforms/project game-width game-height))]
    (->> focus
         (play-cljc.gl.examples-state/rect-example game)
         (play-cljc.gl.example-utils/game-loop
           (fn rand-rects-render [{:keys [entity] :as game}]
             (play-cljc.gl.example-utils/resize-example game)
             (play-cljc.gl.core/render game
               {:clear {:color [1 1 1 1] :depth 1}})
             (play-cljc.gl.core/render game entity)
             game)))))

