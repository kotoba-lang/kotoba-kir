(ns kotoba.kir-trap-source-test
  "T3.3: fuel-exhausted traps cite function + call-stack tip."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.kir :as kir]))

(defn- deep-loop-kir
  "Self-recursive helper that never bottoms out within default fuel."
  []
  {:format :kotoba.kir/v4
   :exports ['main]
   :entry 'main
   :effects #{}
   :functions
   [{:name 'main
     :params []
     :body (list 'spin 0)}
    {:name 'spin
     :params ['n]
     :body (list 'spin (list '+ 'n 1))}]})

(deftest fuel-exhausted-cites-function-and-call-stack
  (try
    (kir/execute (deep-loop-kir) 'main [])
    (is false "expected fuel trap")
    (catch clojure.lang.ExceptionInfo e
      (let [d (ex-data e)]
        (is (= :fuel-exhausted (:trap d)))
        (is (= :ir (:phase d)))
        (is (= 'spin (:function d)))
        (is (vector? (:call-stack d)))
        (is (some #{'spin} (:call-stack d)))
        (is (string? (:hint d)))))))
