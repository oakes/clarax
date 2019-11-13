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
            #?(:clj  [play-cljc.state.macros-java :refer [deffact ->state ->fact]]
               :cljs [play-cljc.state.macros-js :refer-macros [deffact ->state ->fact]]))
  #?(:cljs (:require-macros [dynadoc.example :refer [defexample]])))

(deffact Rect [x y width height])
(deffact Game [width height])

(def *state
  (atom
    (->state
      {:get-rects [Rect]

       :right-boundary
       [:rule
        [?game <- Game]
        [?rect <- Rect (> (+ x width) (:width ?game))]
        =>
        (state/update! ?rect {:x (- (:width ?game) (:width ?rect))})]

       :bottom-boundary
       [:rule
        [?game <- Game]
        [?rect <- Rect (> (+ y height) (:height ?game))]
        =>
        (state/update! ?rect {:y (- (:height ?game) (:height ?rect))})]})))

(swap! *state state/insert! (->fact Rect 50 50 100 100))

;; rect

(defn rect-example [game]
  (gl game disable (gl game CULL_FACE))
  (gl game disable (gl game DEPTH_TEST))
  (swap! *state state/insert! (->fact Game (eu/get-width game) (eu/get-height game)))
  (let [*mouse-state (atom {})]
    (add-watch *mouse-state :mouse-moved
               (fn [_ _ _ new-mouse-state]
                 (swap! *state
                        (fn [state]
                          (let [fact (state/query state Rect)]
                            (state/update! state fact (select-keys new-mouse-state [:x :y])))))))
    (eu/listen-for-mouse game *mouse-state))
  (->> (assoc (e/->entity game primitives/rect)
              :clear {:color [1 1 1 1] :depth 1})
       (c/compile game)
       (assoc game :entity)))

(defn get-rect [state]
  (state/query state Rect))

(defexample play-cljc.state/rect-example
  {:with-card card
   :with-focus [focus (play-cljc.gl.core/render game
                        (-> (assoc entity :viewport {:x 0 :y 0
                                                     :width game-width
                                                     :height game-height})
                            (play-cljc.transforms/project game-width game-height)
                            (play-cljc.transforms/color [1 0 0 1])
                            (play-cljc.transforms/translate x y)
                            (play-cljc.transforms/scale width height)))]}
  (->> (play-cljc.gl.example-utils/init-example card)
       (play-cljc.gl.examples-state/rect-example)
       (play-cljc.gl.example-utils/game-loop
         (fn rect-render [{:keys [entity] :as game}]
           (play-cljc.gl.example-utils/resize-example game)
           (println (count (play-cljc.state/query @play-cljc.gl.examples-state/*state :get-rects)))
           (let [{:keys [x y width height]} (play-cljc.gl.examples-state/get-rect @play-cljc.gl.examples-state/*state)]
             (let [game-width (play-cljc.gl.example-utils/get-width game)
                   game-height (play-cljc.gl.example-utils/get-height game)]
               focus))
           game))))

