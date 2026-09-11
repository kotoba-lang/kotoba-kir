(ns kotoba.kir-zero-charge-loop-test
  "T7.1: loop-helper self-tail is zero-charge after first entry."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.kir :as ir]))

(defn- deep-loop-kir [n]
  {:format :kotoba.kir/v4
   :entry 'main
   :exports ['main]
   :effects #{}
   :functions
   [{:name 'main
     :params []
     :param-types []
     :result :i64
     :effects #{}
     :body (list '__kotoba_loop_1 n)}
    {:name '__kotoba_loop_1
     :params ['n]
     :param-types [:i64]
     :result :i64
     :effects #{}
     :body (list 'if (list '<= 'n 0)
                 42
                 (list '__kotoba_loop_1 (list '- 'n 1)))}]})

(deftest ten-k-loop-fits-default-fuel
  ;; main + first helper entry = 2 units; re-entries free under T7.1.
  (is (= 42 (ir/execute (deep-loop-kir 10000) 'main [] {:fuel 16}))))

(deftest non-helper-recursion-still-charges
  (let [kir {:format :kotoba.kir/v4
             :entry 'main
             :exports ['main]
             :effects #{}
             :functions
             [{:name 'main
               :params []
               :param-types []
               :result :i64
               :effects #{}
               :body (list 'forever 0)}
              {:name 'forever
               :params ['x]
               :param-types [:i64]
               :result :i64
               :effects #{}
               :body (list 'forever 'x)}]}]
    (try
      (ir/execute kir 'main [] {:fuel 32})
      (is false "expected fuel trap")
      (catch clojure.lang.ExceptionInfo e
        (is (= :fuel-exhausted (:trap (ex-data e))))))))
