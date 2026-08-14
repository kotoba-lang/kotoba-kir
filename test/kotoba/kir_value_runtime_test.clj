(ns kotoba.kir-value-runtime-test
  (:require [clojure.test :refer [deftest is]]
            [kotoba.kir :as kir]
            [kotoba.kir.value :as value]))

(defn- module [result body]
  {:format :kotoba.kir/v4
   :entry 'main
   :effects #{}
   :functions [{:name 'main :params [] :param-types [] :result result
                :effects #{} :body body}]})

(defn- problem-of [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e
         (or (:problem (ex-data e)) (:trap (ex-data e))
             (:phase (ex-data e))))))

(deftest value-runtime-ops-use-a-separate-host-bound-dispatch
  (let [calls (atom [])
        encoded (byte-array [1 2 3])
        value-call (fn [op payload]
                     (swap! calls conj [op payload])
                     (case op
                       :value/intern 17
                       :value/hydrate 18
                       :value/resolve encoded
                       :value/cid-of "bafyreicid"
                       :value/release 1))]
    (is (= 17 (kir/execute (module :i64 '(value-intern (bytes-empty)))
                           'main [] {:value-call value-call})))
    (is (= 18 (kir/execute (module :i64 '(value-hydrate "bafyreicid"))
                           'main [] {:value-call value-call})))
    (is (= [1 2 3]
           (vec (kir/execute (module :bytes '(value-resolve 17))
                             'main [] {:value-call value-call}))))
    (is (= "bafyreicid"
           (kir/execute (module :string '(value-cid-of 17))
                        'main [] {:value-call value-call})))
    (is (= 1 (kir/execute (module :i64 '(value-release 17))
                          'main [] {:value-call value-call})))
    (is (= [:value/intern :value/hydrate :value/resolve
            :value/cid-of :value/release]
           (mapv first @calls)))))

(deftest value-runtime-ops-fail-closed
  (is (= :value-runtime-unavailable
         (problem-of #(kir/execute (module :i64 '(value-intern (bytes-empty)))
                                   'main []))))
  (is (= :value-runtime-arity
         (problem-of #(kir/execute (module :i64 '(value-release 1 2))
                                   'main [] {:value-call (fn [_ _] 1)}))))
  (is (= :value-type-mismatch
         (problem-of #(kir/execute (module :i64 '(value-intern (bytes-empty)))
                                   'main [] {:value-call (fn [_ _] "forged")}))))
  (is (= :invalid-parametric-value
         (problem-of #(kir/execute (module :bytes '(value-resolve 1))
                                   'main [] {:value-call (fn [_ _] :not-bytes)}))))
  (is (= :ir
         (problem-of #(kir/execute (module :i64 '(value-release 1))
                                   'main [] {:value-call :not-a-function})))))

(deftest value-runtime-operations-neither-mint-nor-replace-capabilities
  (is (= :value/intern
         (get-in kir/value-runtime-operations ['value-intern :abi-op])))
  (is (every? kir/non-string-typed-ops (keys kir/value-runtime-operations))
      "native/CLJS targets stay closed until they implement the ABI and its authority binding")
  (is (value/bytes-value?
       (kir/execute (module :bytes '(value-resolve 1)) 'main []
                    {:value-call (fn [_ _] (byte-array 0))}))))

(deftest production-native-stays-closed-until-the-aiueos-provider-is-qualified
  (doseq [[result body] [[:i64 '(value-intern (bytes-empty))]
                         [:i64 '(value-hydrate "bafyreicid")]
                         [:bytes '(value-resolve 1)]
                         [:string '(value-cid-of 1)]
                         [:i64 '(value-release 1)]]]
    (is (false? (kir/only-native-word-typed-features? (module result body)))
        (str body))))
