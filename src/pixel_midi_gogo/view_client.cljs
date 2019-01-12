(ns pixel-midi-gogo.view-client
  (:require [pixel-midi-gogo.core :as pmg-core]
            [pixel-midi-gogo.utils :as utils]
            [rum.core :as rum]
            [clojure.walk :as walk]))

(defn on-mount [state]
  (let [[_ view] (:rum/args state)]
    (@pmg-core/*send-action-fn "view-mount" view))
  state)

(defn on-unmount [state]
  (let [[_ view] (:rum/args state)]
    (@pmg-core/*send-action-fn "view-unmount" view))
  state)

(rum/defc empty-comp
  < {:did-mount on-mount
     :after-render on-mount
     :did-remount (fn [_ state]
                    (on-mount state))
     :will-unmount on-unmount}
  [content view]
  content)

(defn update-attrs [x]
  (if (and (vector? x)
           (map? (second x)))
    (update x 1
      (fn [attrs]
        (reduce
          (fn [new-attrs [k v]]
            (assoc new-attrs
              k (if (.startsWith (name k) "on-")
                  (partial utils/add-event v)
                  v)))
          {}
          attrs)))
    x))

(defmethod pmg-core/action
  "view-insert"
  [_ {:keys [parent value] :as view}]
  (if-let [elem (.querySelector js/document parent)]
    (-> (walk/prewalk update-attrs value)
        (empty-comp view)
        (rum/mount elem))
    (throw (js/Error. (str "Couldn't find " parent)))))
