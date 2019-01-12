(ns pixel-midi-gogo.view
  (:require [pixel-midi-gogo.core :as pmg-core]
            #?(:cljs [pixel-midi-gogo.view-client])))

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

(defmethod pmg-core/insert
  View
  [& args]
  (@pmg-core/*send-action-fn "view-insert" (last args)))

