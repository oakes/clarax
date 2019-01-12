(ns pixel-midi-gogo.canvas-client
  (:require [pixel-midi-gogo.core :as pmg-core]
            [play-cljs.core :as p]))

(defonce *elem->game (atom {}))

(defmethod pmg-core/action
  "canvas-insert"
  [_ {:keys [parent] :as canvas}]
  (if-let [elem (.querySelector js/document parent)]
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
      (reset! *content (:value canvas)))
    (throw (js/Error. (str "Couldn't find " parent)))))
