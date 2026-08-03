(ns kotoba.kir-closure-param-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.kir :as kir]))

(defn- hir-with-consumer [closure-expr]
  {:format :kotoba.hir/v3
   :entry 'main
   :exports ['main]
   :result :i64
   :effects #{}
   :functions
   [{:name 'main :params [] :param-types [] :result :i64 :effects #{}
     :body (list 'let ['closure closure-expr] '(consume closure))}
    {:name 'consume :params ['closure] :param-types [:i64]
     :closure-param-indexes [0]
     :result :i64 :effects #{} :body '(pair-first closure)}]})

(deftest lower-preserves-and-executes-closure-parameter-refinements
  (let [lowered (kir/lower (hir-with-consumer '(pair 7 0)))
        consumer (some #(when (= 'consume (:name %)) %) (:functions lowered))]
    (is (= [0] (:closure-param-indexes consumer)))
    (is (= 7 (kir/execute lowered 'main [])))))

(deftest malformed-closure-parameter-refinements-fail-closed
  (doseq [indexes [[1] [0 0] [0 -1] ["0"]]]
    (testing (pr-str indexes)
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"closure parameter indexes are malformed"
           (kir/lower
            (assoc-in (hir-with-consumer '(pair 7 0))
                      [:functions 1 :closure-param-indexes] indexes)))))))

(deftest closure-parameter-runtime-shape-is-checked
  (testing "an ordinary i64 cannot masquerade as a closure handle"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"invalid-pair-handle"
         (kir/lower (hir-with-consumer 7)))))
  (testing "the capture tail must be a bounded pair-chain"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"invalid-pair-handle"
         (kir/lower (hir-with-consumer '(pair 7 9)))))))
