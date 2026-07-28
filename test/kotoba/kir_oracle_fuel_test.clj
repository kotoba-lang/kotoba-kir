(ns kotoba.kir-oracle-fuel-test
  "T7.4: loop-helper self-tail trampoline + raised oracle fuel."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.kir :as ir]))

(defn- deep-loop-hir [n]
  {:format :kotoba.hir/v3
   :entry 'main
   :exports ['main]
   :effects #{}
   :result :i64
   :schemas {}
   :schema-identities {}
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

(deftest lower-oracle-and-execute-10k-loop
  (let [kir (ir/lower (deep-loop-hir 10000))]
    (is (map? kir))
    (is (= 42 (:oracle-value kir))
        "oracle-fuel + trampoline should constant-fold 10k loop")
    (is (= 42 (ir/execute kir 'main [] {:fuel 12000})))))

(deftest execute-10k-loop-with-raised-fuel
  (let [kir (ir/lower (deep-loop-hir 10000))]
    (is (= 42 (ir/execute kir 'main [] {:fuel 12000})))))
