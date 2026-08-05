(ns kotoba.kir-bool-parameter-test
  "A `:bool` at a function boundary, in the parameter direction.

  ADR 0219 admitted `:i64`/`:string`/`:keyword`, `[:option T]`/`[:result T E]`
  and records at the native boundary but held `:bool` back, on the ground that
  `execute` could not run one: it recorded
  `{:trap :value-type-mismatch :expected :i64 :position {:parameter b}}` and
  concluded the gap was in the interpreter.

  The trap reproduces -- `the-untyped-encoding-still-loses-a-bool-parameter`
  below pins it byte for byte -- but it is not the interpreter refusing a
  `:bool` parameter. It is the interpreter refusing a host boolean where the
  KIR it was handed DECLARES `:i64`, because that KIR carries no
  `:param-types` table and an absent one defaults to `:i64` per parameter.

  So these tests do the thing ADR 0219 asked for and would not assume: they
  EXECUTE a `:bool` parameter, in every position one can occupy, before the
  admission gate is allowed to admit one."
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.kir :as kir]))

(defn- module
  "A one-function typed module whose single parameter `b` is a `:bool`."
  ([body result] (module body result [:bool]))
  ([body result param-types]
   {:format :kotoba.kir/v4
    :entry 'f
    :exports ['f]
    :functions [{:name 'f :params ['b] :param-types param-types
                 :result result :body body}]}))

(defn- run [program args]
  (kir/execute program 'f args {:fuel 10000}))

;; ---------------------------------------------------------------------------
;; The interpreter executes one
;; ---------------------------------------------------------------------------

(deftest a-bool-parameter-crosses-the-entry-boundary
  (let [program (module '(if b 1 0) :i64)]
    (is (= 1 (run program [true])))
    (is (= 0 (run program [false])))))

(deftest a-bool-parameter-is-a-host-boolean-at-the-boundary
  ;; The convention this stack already settled (`kotoba-kir` 38d1bd0): `:bool`
  ;; is a plain 0/1 word INSIDE a module, but the value that crosses a target
  ;; boundary is a host boolean. `box-bool` enforces it on the way out; the
  ;; argument check enforces it on the way in. Both directions, one rule.
  ;;
  ;; This negative is the half that is easy to lose: admitting the word `1`
  ;; here would make the interpreter accept a spelling no other target's entry
  ;; boundary produces, and the shared corpora would stop comparing.
  (let [program (module '(if b 1 0) :i64)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"argument must be a boolean"
                          (run program [1])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"argument must be a boolean"
                          (run program [0])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"argument must be a boolean"
                          (run program ["true"])))))

(deftest a-bool-parameter-drives-the-bool-operations
  (testing "if test"
    (is (= 7 (run (module '(if b 7 8) :i64) [true]))))
  (testing "bool-not"
    ;; `bool-not` decodes both spellings through `kotoba-false?`, which is why
    ;; a host boolean parameter reaches it safely.
    (is (= 0 (run (module '(if (bool-not b) 1 0) :i64) [true])))
    (is (= 1 (run (module '(if (bool-not b) 1 0) :i64) [false]))))
  (testing "let rebinding"
    (is (= 7 (run (module '(let [x b] (if x 7 8)) :i64) [true])))
    (is (= 8 (run (module '(let [x b] (if x 7 8)) :i64) [false]))))
  (testing "equality against itself"
    (is (= 1 (run (module '(if (= b b) 1 0) :i64) [true])))))

(deftest a-bool-parameter-returns-as-a-boxed-bool-result
  ;; Parameter in, result out, same value: the two boundary directions compose.
  (let [program (module 'b :bool)]
    (is (true? (run program [true])))
    (is (false? (run program [false]))))
  (let [program (module '(bool-not b) :bool)]
    (is (false? (run program [true])))
    (is (true? (run program [false])))))

(deftest a-bool-parameter-crosses-an-internal-call-boundary
  ;; The entry boundary is not the only place a `:bool` parameter is bound --
  ;; `invoke-function` validates every call. This is the position ADR 0219's
  ;; measurement actually failed in.
  (let [program {:format :kotoba.kir/v4 :entry 'f :exports ['f]
                 :functions [{:name 'g :params ['x] :param-types [:bool]
                              :result :i64 :body '(if x 10 20)}
                             {:name 'f :params ['b] :param-types [:bool]
                              :result :i64 :body '(g b)}]}]
    (is (= 10 (kir/execute program 'f [true] {:fuel 10000})))
    (is (= 20 (kir/execute program 'f [false] {:fuel 10000})))))

(deftest a-bool-parameter-populates-a-bool-record-field
  ;; `native-word-field-types` has always admitted a `:bool` record FIELD. Until
  ;; now nothing could supply one from a parameter, so the two features had
  ;; never met.
  (let [record-type [:record :kir.bool-test/flagged [[:flag :bool] [:n :i64]]]
        program (module (list 'record-get record-type
                              (list 'record-new record-type 'b 5) :flag)
                        :bool)]
    (is (true? (kir/execute program 'f [true] {:fuel 10000})))
    (is (false? (kir/execute program 'f [false] {:fuel 10000}))))
  (let [record-type [:record :kir.bool-test/flagged [[:flag :bool] [:n :i64]]]
        program (module (list 'record-get record-type
                              (list 'record-new record-type 'b 5) :n)
                        :i64)]
    (is (= 5 (kir/execute program 'f [true] {:fuel 10000})))))

(deftest a-bool-parameter-sits-beside-other-boundary-types
  ;; The shape this admission gate actually governs. A module with a `:bool`
  ;; parameter AND another typed feature is the one that keeps its
  ;; `:param-types` through `lower`, and the one the native target refused.
  (let [program {:format :kotoba.kir/v4 :entry 'f :exports ['f]
                 :functions [{:name 'f :params ['s 'b] :param-types [:string :bool]
                              :result :string
                              :body '(if b (string-concat s "-on")
                                         (string-concat s "-off"))}]}]
    (is (= "sw-on" (kir/execute program 'f ["sw" true] {:fuel 10000})))
    (is (= "sw-off" (kir/execute program 'f ["sw" false] {:fuel 10000})))))

;; ---------------------------------------------------------------------------
;; The gate admits one, now that the oracle runs one
;; ---------------------------------------------------------------------------

(deftest the-native-gate-admits-a-bool-parameter
  (let [hir {:format :kotoba.hir/v3 :entry 'main :exports ['main 'label]
             :functions [{:name 'label :params ['s 'b] :param-types [:string :bool]
                          :result :string
                          :body '(if b (string-concat s "-on")
                                     (string-concat s "-off"))}
                         {:name 'main :params [] :param-types [] :result :i64
                          :body '(if 1 1 0)}]}]
    (is (true? (kir/only-native-word-typed-features? hir))
        "a :bool parameter is a one-word boundary type like every other scalar here")))

(deftest the-native-gate-still-refuses-what-it-always-refused
  ;; Widening one type must not widen the set by accident. `:f64` is a word too,
  ;; but it is not a word this boundary carries.
  (let [hir {:format :kotoba.hir/v3 :entry 'main :exports ['main 'g]
             :functions [{:name 'g :params ['x] :param-types [:f64] :result :i64
                          :body '(if 1 1 0)}
                         {:name 'main :params [] :param-types [] :result :i64
                          :body '(if 1 1 0)}]}]
    (is (false? (kir/only-native-word-typed-features? hir)))))

;; ---------------------------------------------------------------------------
;; What is still not closed, measured rather than described
;; ---------------------------------------------------------------------------

(deftest the-untyped-encoding-still-loses-a-bool-parameter
  ;; ADR 0219's exact trap, reproduced and pinned. `lower` keeps `:param-types`
  ;; only for `:kotoba.hir/v3`; an HIR without that format loses the table, and
  ;; every parameter then declares `:i64`.
  ;;
  ;; This is deliberately a REGRESSION test for a gap that is NOT closed here:
  ;; the classification that sends a `:bool`-only module down this path lives in
  ;; `kotoba.compiler.frontend`, and changing what `lower` keeps would move
  ;; `:kir-sha256` for every affected module on every target. When that follow-on
  ;; lands, this test is the one that must be updated -- deliberately, with the
  ;; digest movement counted.
  (let [hir {:entry 'main :result :i64 :exports ['main]
             :functions [{:name 'g :params ['b] :param-types [:bool] :result :i64
                          :body '(if b 1 0)}
                         {:name 'main :params [] :param-types [] :result :i64
                          :body '(g true)}]}
        lowered (kir/lower (assoc hir :format :kotoba.hir/v3))]
    (testing "v3 keeps the table and the oracle folds"
      (is (= [:bool] (:param-types (first (:functions lowered)))))
      (is (= 1 (:oracle-value lowered))))
    (testing "without it, a host boolean is refused as an i64"
      (try
        (kir/lower hir)
        (is false "expected the ADR 0219 trap")
        (catch clojure.lang.ExceptionInfo error
          (let [data (ex-data error)]
            (is (= :value-type-mismatch (:trap data)))
            (is (= :i64 (:expected data)))
            (is (= {:function 'g :parameter 'b} (:position data)))))))))
