(ns play-cljc.gl.examples-clarax
  (:require [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.gl.example-utils :as eu]
            [play-cljc.gl.example-data :as data]
            [play-cljc.transforms :as t]
            [play-cljc.instances :as i]
            [play-cljc.primitives-2d :as primitives]
            [clara.rules :as clara]
            [clara.rules.accumulators :as acc]
            [clarax.rules :as clarax]
            #?(:clj  [play-cljc.macros-java :refer [gl]]
               :cljs [play-cljc.macros-js :refer-macros [gl]])
            #?(:clj [dynadoc.example :refer [defexample]])
            #?(:clj  [clarax.macros-java :refer [->session]]
               :cljs [clarax.macros-js :refer-macros [->session]]))
  #?(:cljs (:require-macros [dynadoc.example :refer [defexample]])))

(defrecord Rect [x y width height])
(defrecord Game [width height])

(def *state
  (-> {:get-rect
       (fn []
         (let [rect Rect]
           rect))
       :get-rects
       (fn []
         (let [rect Rect
               :accumulator (acc/all)]
           rect))
       :right-boundary
       (let [game Game
             rect Rect
             :when (> (+ (:x rect) (:width rect))
                      (:width game))]
         (clarax/merge! ?rect {:x (- (:width game) (:width rect))}))
       :bottom-boundary
       (let [game Game
             rect Rect
             :when (> (+ (:y rect) (:height rect))
                      (:height game))]
         (clarax/merge! rect {:y (- (:height game) (:height rect))}))}
      ->session
      (clara/insert (->Rect 50 50 100 100))
      clara/fire-rules
      atom))

;; rect

(defn rect-example [game]
  (gl game disable (gl game CULL_FACE))
  (gl game disable (gl game DEPTH_TEST))
  (swap! *state (fn [state]
                  (-> state
                      (clara/insert (->Game (eu/get-width game) (eu/get-height game)))
                      clara/fire-rules)))
  (let [*mouse-state (atom {})]
    (add-watch *mouse-state :mouse-moved
               (fn [_ _ _ new-mouse-state]
                 (swap! *state
                        (fn [state]
                          (as-> state $
                                (clara/query $ :get-rect)
                                (clarax/merge state $ (select-keys new-mouse-state [:x :y]))
                                (clara/fire-rules $))))))
    (eu/listen-for-mouse game *mouse-state))
  (->> (assoc (e/->entity game primitives/rect)
              :clear {:color [1 1 1 1] :depth 1})
       (c/compile game)
       (assoc game :entity)))

(defexample rect-example
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
       (play-cljc.gl.examples-clarax/rect-example)
       (play-cljc.gl.example-utils/game-loop
         (fn rect-render [{:keys [entity] :as game}]
           (play-cljc.gl.example-utils/resize-example game)
           (println (count (clara.rules/query @play-cljc.gl.examples-clarax/*state :get-rects)))
           (let [{:keys [x y width height]} (clara.rules/query @play-cljc.gl.examples-clarax/*state :get-rect)]
             (let [game-width (play-cljc.gl.example-utils/get-width game)
                   game-height (play-cljc.gl.example-utils/get-height game)]
               focus))
           game))))

