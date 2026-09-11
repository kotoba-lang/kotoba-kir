(ns kotoba.kir-typed-list-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.kir :as kir]))

(defn- module [body]
  {:format :kotoba.kir/v4
   :entry 'main
   :effects #{}
   :functions [{:name 'main :params [] :param-types [] :result :i64
                :effects #{} :body body}]})

(deftest canonical-typed-lists-construct-and-count
  (is (= 3
         (kir/execute
          (module '(vector-count (typed-list-new [:list :i64] 4 5 6)))
          'main []))))

(deftest canonical-typed-lists-fail-closed
  (testing "items are checked against the declared descriptor"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"signed i64"
         (kir/execute
          (module '(vector-count (typed-list-new [:list :i64] 4 false)))
          'main []))))
  (testing "the constructor requires a list descriptor"
    (is (thrown? clojure.lang.ExceptionInfo
                 (kir/execute
                  (module '(typed-list-new [:vector [:i64]] 4))
                  'main [])))))
