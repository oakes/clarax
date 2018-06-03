(ns pixel-midi-gogo.view
  (:require [clara.rules :as rules :refer [defrule]]
            [rum.core :as rum]
            [clojure.walk :as walk]
            [pixel-midi-gogo.core :refer [*session edit]]
            [pixel-midi-gogo.event :refer [add-event]]
            [clara.rules.accumulators :as acc]))

(defrecord View [parent value mounted? timestamp])

(defn on-mount [state]
  (let [[_ view] (:rum/args state)]
    (when-not (:mounted? view)
      (let [edit-view (fn [session]
                        (-> session
                            (edit view {:mounted? true})
                            rules/fire-rules))]
        (if @*session
          (swap! *session edit-view)
          (add-watch *session (pr-str view)
            (fn [_ _ old-session new-session]
              (when (and (nil? old-session)
                         (some? new-session))
                (swap! *session edit-view))))))))
  state)

(rum/defc empty-comp
  < {:did-mount on-mount
     :did-remount (fn [_ state]
                    (on-mount state))}
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

(defrule views
  [?view <- (acc/max :timestamp :returns-fact true)
   :from [View (= ?parent parent)]]
  =>
  (if-let [elem (.querySelector js/document ?parent)]
    (-> (walk/prewalk update-attrs (:value ?view))
        (empty-comp ?view)
        (rum/mount elem))
    (throw (js/Error. (str "Couldn't find " ?parent)))))

