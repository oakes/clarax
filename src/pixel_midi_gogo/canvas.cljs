(ns pixel-midi-gogo.canvas
  (:require [clara.rules :refer [defrule]]
            [clara.rules.accumulators :as acc]
            [play-cljs.core :as p]))

(defrecord Canvas [parent value timestamp])

(defonce ^:private *game (atom nil))

(defrule canvases
  [?canvas <- (acc/max :timestamp :returns-fact true)
   :from [Canvas (= ?parent parent)]]
  =>
  (if-let [elem (.querySelector js/document ?parent)]
    (let [game (or @*game
                   (reset! *game (p/create-game (.-clientWidth elem) (.-clientHeight elem)
                                   {:parent elem})))]
      (doto game
        (p/start)
        (p/listen "resize"
          (fn [event]
            (p/set-size game (.-clientWidth elem) (.-clientHeight elem))))
        (p/set-screen
          (reify p/Screen
            (on-show [this])
            (on-hide [this])
            (on-render [this]
              (p/render game (:value ?canvas)))))))
    (throw (js/Error. (str "Couldn't find " ?parent)))))

