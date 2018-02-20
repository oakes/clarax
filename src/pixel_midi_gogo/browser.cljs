(ns pixel-midi-gogo.browser
  (:require [clara.rules :as rules :refer [defrule]]
            [rum.core :as rum]
            [clojure.walk :as walk]
            [pixel-midi-gogo.core :refer [insert *session]]))

(defrecord Element [parent value])

(defrecord Event [id type options])

(rum/defc empty-comp
  [content]
  content)

(defn jsx->clj
  [x]
  (into {} (for [k (.keys js/Object x)] [k (aget x k)])))

(defn add-event [id e]
  (let [{:strs [type] :as opts} (jsx->clj e)]
    (swap! *session
      (fn [session]
        (-> session
            (insert (keyword type (name id)) (->Event id type opts))
            rules/fire-rules)))))

(defrule elem
  [Element (= ?parent parent) (= ?value value)]
  =>
  (-> (walk/postwalk
        (fn [x]
          (if (and (vector? x)
                   (map? (second x))
                   (:id (second x)))
            (update x 1
              (fn [attrs]
                (reduce
                  (fn [new-attrs [k v]]
                    (assoc new-attrs
                      k (if (.startsWith (name k) "on-")
                          (partial add-event (:id attrs))
                          v)))
                  {}
                  attrs)))
            x))
        ?value)
      empty-comp
      (rum/mount (.querySelector js/document ?parent))))

