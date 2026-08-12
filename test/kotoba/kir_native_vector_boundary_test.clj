(ns kotoba.kir-native-vector-boundary-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.kir :as kir]))

(defn- hir [vector-type exported-helper?]
  {:format :kotoba.hir/v3
   :entry 'main
   :exports (cond-> ['main] exported-helper? (conj 'retain-vector))
   :functions [{:name 'retain-vector :params ['values]
                :param-types [vector-type] :result vector-type :body 'values}
               {:name 'main :params [] :param-types [] :result :i64
                :body (list (if (= :vector-f64 vector-type)
                              'vector-f64-count 'vector-count)
                            (list 'retain-vector
                                  (list (if (= :vector-f64 vector-type)
                                          'vector-f64-new 'vector-new)
                                        1)))}]})

(deftest native-vector-handles-cross-private-function-boundaries
  (doseq [vector-type [:vector-i64 :vector-f64]]
    (testing (name vector-type)
      (is (true? (kir/only-native-word-typed-features?
                  (hir vector-type false)))))))

(deftest native-vector-handles-do-not-cross-kexe-export-parameter-boundaries
  (doseq [vector-type [:vector-i64 :vector-f64]]
    (testing (name vector-type)
      (is (false? (kir/only-native-word-typed-features?
                   (hir vector-type true)))))))

(deftest native-vector-results-may-cross-a-non-entry-export-by-copy
  (doseq [vector-type [:vector-i64 :vector-f64]]
    (testing (name vector-type)
      (let [constructor (if (= :vector-f64 vector-type)
                          'vector-f64-new 'vector-new)
            module {:format :kotoba.hir/v3 :entry 'main
                    :exports ['main 'values]
                    :functions [{:name 'values :params [] :param-types []
                                 :result vector-type
                                 :body (list constructor 1 2)}
                                {:name 'main :params [] :param-types []
                                 :result :i64 :body 0}]}]
        (is (true? (kir/only-native-word-typed-features? module)))))))

(deftest native-vector-export-copy-v1-is-zero-arity
  (doseq [vector-type [:vector-i64 :vector-f64]]
    (let [constructor (if (= :vector-f64 vector-type)
                        'vector-f64-new 'vector-new)
          module {:format :kotoba.hir/v3 :entry 'main
                  :exports ['main 'values]
                  :functions [{:name 'values :params ['value]
                               :param-types [:i64] :result vector-type
                               :body (list constructor 'value)}
                              {:name 'main :params [] :param-types []
                               :result :i64 :body 0}]}]
      (is (false? (kir/only-native-word-typed-features? module))))))

(deftest native-string-index-handles-are-private-too
  (let [module {:format :kotoba.hir/v3
                :entry 'main
                :exports ['main]
                :functions
                [{:name 'retain-index :params ['index]
                  :param-types [:string-index] :result :string-index
                  :body 'index}
                 {:name 'main :params [] :param-types [] :result :i64
                  :body '(string-index-count
                          (retain-index
                           (string-index-assoc
                            (string-index-new) "cid" 7)))}]}]
    (is (true? (kir/only-native-word-typed-features? module)))
    (is (false? (kir/only-native-word-typed-features?
                 (update module :exports conj 'retain-index))))
    (is (false? (kir/only-native-word-typed-features?
                 (assoc-in module [:functions 1 :body]
                           '(string-index-assoc (string-index-new) "cid")))))))
