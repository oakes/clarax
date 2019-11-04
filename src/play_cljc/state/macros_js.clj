(ns play-cljc.state.macros-js
  (:require [play-cljc.state.build :as build]
            [cljs.env :as env]
            [clara.macros :as macros]
            [clara.rules :as rules]
            [clara.rules.engine :as eng]))

(def ^:private ^:const ns-sym 'play-cljc.state)

(defn- cljs-fn
  ([val]
   (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym] val))
  ([key val]
   (swap! env/*compiler* assoc-in [:clara.macros/productions ns-sym key] val)))

(defmacro defsession [var-name & forms]
  (binding [build/*cljs-fn* cljs-fn]
    (let [{:keys [init-forms queries]} (build/forms->rules var-name forms)
          session (macros/sources-and-options->session-assembly-form [(list 'quote ns-sym)])]
      `(let [var# (def ~var-name (atom nil))]
         ~(cons 'do
            (for [[sym query] queries]
              `(def ~sym ~query)))
           (->> ~session ~@init-forms rules/fire-rules (reset! ~var-name))
           var#))))

(defmacro query [session query & params]
  `(-> (eng/query ~session ~query ~(apply hash-map params))
       first
       :?ret))

