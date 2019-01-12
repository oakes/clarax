(ns pixel-midi-gogo.view
  (:require [pixel-midi-gogo.core :as pmg-core]
            [clara.rules.accumulators :as acc]
            #?@(:cljs [[pixel-midi-gogo.event :refer [add-event]]
                       [rum.core :as rum]
                       [clojure.walk :as walk]])))

(defrecord View [parent value timestamp])

(defmethod pmg-core/action
  "view-mount"
  [_ view]
  (let [id (pr-str view)]
    (if @pmg-core/*session
      (swap! pmg-core/*session pmg-core/insert* view)
      (add-watch pmg-core/*session id
        (fn [_ _ old-session new-session]
          (when (and (nil? old-session)
                     (some? new-session))
            (swap! pmg-core/*session pmg-core/insert* view)
            (remove-watch pmg-core/*session id)))))))

(defmethod pmg-core/action
  "view-unmount"
  [_ view]
  (swap! pmg-core/*session pmg-core/delete view))

#?(:cljs [(defn on-mount [state]
            (let [[_ view] (:rum/args state)]
              (pmg-core/send-action "view-mount" view))
            state)
          
          (defn on-unmount [state]
            (let [[_ view] (:rum/args state)]
              (pmg-core/send-action "view-unmount" view))
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
          
          (defmethod pmg-core/action
            "view-insert"
            [_ {:keys [parent value] :as view}]
            (if-let [elem (.querySelector js/document parent)]
              (-> (walk/prewalk update-attrs value)
                  (empty-comp view)
                  (rum/mount elem))
              (throw (js/Error. (str "Couldn't find " parent)))))])

(defmethod pmg-core/insert
  View
  [& args]
  (pmg-core/send-action "view-insert" (last args)))

