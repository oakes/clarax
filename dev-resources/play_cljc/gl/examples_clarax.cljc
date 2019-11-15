(ns play-cljc.gl.examples-clarax
  (:require [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.gl.example-utils :as eu]
            [play-cljc.gl.example-data :as data]
            [play-cljc.transforms :as t]
            [play-cljc.instances :as i]
            [play-cljc.primitives-2d :as primitives]
            [clara.rules :as clara]
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
  (atom
    (->session
      {:get-rect
       (fn []
         (let [rect Rect]
           rect))
       :get-rects
       (fn []
         (let [rect [Rect]]
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
         (clarax/merge! rect {:y (- (:height game) (:height rect))}))})))

(swap! *state clara/insert (->Rect 50 50 100 100))

;; rect

(defn rect-example [game]
  (gl game disable (gl game CULL_FACE))
  (gl game disable (gl game DEPTH_TEST))
  (swap! *state clara/insert (->Game (eu/get-width game) (eu/get-height game)))
  (let [*mouse-state (atom {})]
    (add-watch *mouse-state :mouse-moved
               (fn [_ _ _ new-mouse-state]
                 (swap! *state
                        (fn [state]
                          (let [fact (clara/query state :get-rect)]
                            (clarax/merge state fact (select-keys new-mouse-state [:x :y])))))))
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

