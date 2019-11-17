(ns examples.clara
  (:require [clara.rules :as clara #?@(:clj [:refer [defsession]])]
            [clara.rules.accumulators :as acc])
  #?(:cljs (:require-macros [clara.rules :refer [defsession]])))

(println "Clara")
