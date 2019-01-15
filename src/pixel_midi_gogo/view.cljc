(ns pixel-midi-gogo.view
  (:require [pixel-midi-gogo.core :as pmg-core]
            #?(:cljs [pixel-midi-gogo.view-client :as client])))

(defrecord View [parent value timestamp])
  
(extend-type View
  pmg-core/Insertable
  (insert!
    ([this] (pmg-core/send-action! this "insert"))
    ([this session] (pmg-core/insert! this)))
  pmg-core/ActionReceivable
  (receive-action! [this action-name]
    (case action-name
      "mount"
      (let [id (pr-str this)]
        (if @pmg-core/*session
          (swap! pmg-core/*session (partial pmg-core/insert* this))
          (add-watch pmg-core/*session id
            (fn [_ _ old-session new-session]
              (when (and (nil? old-session)
                         (some? new-session))
                (swap! pmg-core/*session (partial pmg-core/insert* this))
                (remove-watch pmg-core/*session id))))))
      "unmount"
      (swap! pmg-core/*session (partial pmg-core/delete! this))
      #?@(:cljs ["insert" (client/insert! this)])
      (pmg-core/receive-action* this action-name))))

