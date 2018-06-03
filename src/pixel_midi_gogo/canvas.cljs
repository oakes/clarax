(ns pixel-midi-gogo.canvas
  (:require [clara.rules :refer [defrule]]
            [clara.rules.accumulators :as acc]
            [play-cljs.core :as p]))

(defrecord Canvas [parent value timestamp])

(defonce ^:private *elem->game (atom {}))

(defrule canvases
  [?canvas <- (acc/max :timestamp :returns-fact true)
   :from [Canvas (= ?parent parent)]]
  =>
  (if-let [elem (.querySelector js/document ?parent)]
    (let [{:keys [game *content]}
          (or (get @*elem->game elem)
              (-> *elem->game
                  (swap! assoc elem
                    (let [game (p/create-game (.-clientWidth elem) (.-clientHeight elem)
                                 {:parent elem})
                          *content (atom nil)]
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
                              (some->> @*content
                                       (p/render game))))))
                      {:game game
                       :*content *content}))
                  (get elem)))]
      (reset! *content (:value ?canvas)))
    (throw (js/Error. (str "Couldn't find " ?parent)))))

