(ns pixel-midi-gogo.view
  (:require [clara.rules :as rules :refer [defrule]]
            [rum.core :as rum]
            [clojure.walk :as walk]
            [pixel-midi-gogo.core :as pmg-core :refer [*session]]
            [pixel-midi-gogo.event :refer [add-event]]
            [clara.rules.accumulators :as acc]))

(defrecord View [parent value timestamp])

(defn on-mount [state]
  (let [[_ view] (:rum/args state)
        add-view (fn [session]
                   (-> session
                       (pmg-core/insert* view)
                       rules/fire-rules))]
    (if @*session
      (swap! *session add-view)
      (add-watch *session (pr-str view)
        (fn [_ _ old-session new-session]
          (when (and (nil? old-session)
                     (some? new-session))
            (swap! *session add-view))))))
  state)

(defn on-unmount [state]
  (let [[_ view] (:rum/args state)]
    (swap! *session (fn [session]
                      (-> session
                          (pmg-core/delete view)
                          rules/fire-rules))))
  state)

(rum/defc empty-comp
  < {:did-mount on-mount
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
                  (partial add-event v)
                  v)))
          {}
          attrs)))
    x))

(defmethod pmg-core/insert
  View
  [& args]
  (let [{:keys [parent value] :as view} (last args)]
    (if-let [elem (.querySelector js/document parent)]
      (-> (walk/prewalk update-attrs value)
          (empty-comp view)
          (rum/mount elem))
      (throw (js/Error. (str "Couldn't find " parent))))))

