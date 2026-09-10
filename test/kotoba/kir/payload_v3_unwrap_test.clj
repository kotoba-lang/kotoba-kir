(ns kotoba.kir.payload-v3-unwrap-test
  "The conversion payload v3 needs, written and measured before it is needed.

  `kotoba.kir-value-codec-differential-test` measured, 2026-09-10, that a v3
  which hands `identity-payload` straight to `kotoba.value.v1` does not fail:
  it takes normalize's f64 and i64 WRAPPERS for the ordinary one-entry maps
  they structurally are, encodes the wrapper keyword into the block, and never
  reaches the value model's own float or exact integer. Accepted and wrong,
  which is worse than refused.

  It named the work item -- unwrap into `value/float64` and `value/int64`
  BEFORE encoding -- and stopped there. This is that conversion, plus what it
  takes to believe it.

  ## Why it lives in test/

  ADR-2609076000 decision 1 keeps io-ipld here TEST-ONLY, and says promoting
  it to `:deps` IS the payload v3 decision. A conversion in `src/` would
  require that promotion, so writing one here would be taking the decision by
  accident. Nothing in this namespace changes a byte of any block, and the
  frozen vectors say so in the differential test next door. When v3 is
  actually taken, `unwrap` moves as it stands.

  ## What makes this believable rather than merely green

  The same payload is encoded BOTH ways in every case, and the assertions are
  about the difference. A test that only encoded the unwrapped form would pass
  just as well if `unwrap` were `identity` and the defect did not exist."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [cbor.core :as cbor]
            [ipld.value :as value]
            [kotoba.kir.definition-identity :as identity]))

(def ^:private f64-key :kotoba.lang.code-identity/f64)
(def ^:private i64-key :kotoba.lang.code-identity/i64)

(defn- f64-wrapper? [x]
  (and (map? x) (= 1 (count x)) (string? (get x f64-key))))

(defn- i64-wrapper? [x]
  (and (map? x) (= 1 (count x)) (string? (get x i64-key))))

(defn unwrap
  "normalize's admitted EDN -> the same value with its two wrappers replaced by
  the value model's own float and exact integer.

  Postwalk, so a wrapper nested anywhere in the typed KIR is reached. The
  wrappers are the ONLY thing touched: everything else -- keywords, symbols,
  sets, vectors, strings, bytes -- already has a code and passes through."
  [x]
  (walk/postwalk
   (fn [node]
     (cond
       (f64-wrapper? node)
       (value/float64 (Double/longBitsToDouble (Long/parseUnsignedLong (get node f64-key) 16)))

       (i64-wrapper? node)
       (value/int64 (Long/parseLong (get node i64-key)))

       :else node))
   x))

(defn- form [x] (value/value->form x))
(defn- hex [bs] (apply str (map #(format "%02x" (bit-and % 0xff)) (seq bs))))
(defn- bytes-of [x] (hex (cbor/encode (form x))))

(def ^:private wrapper-keyword-hex
  (hex (.getBytes "kotoba.lang.code-identity" "UTF-8")))

(def ^:private vectors
  (-> "kotoba/kir/fixtures/code-identity-vectors.edn"
      io/resource
      (or (io/file "test/kotoba/kir/fixtures/code-identity-vectors.edn"))
      slurp
      edn/read-string
      :vectors))

(deftest an-f64-reaches-the-value-models-float-only-after-unwrapping
  (let [wrapped (identity/f64 1.5)]
    (testing "before"
      (is (= value/code-map (first (form wrapped))))
      (is (clojure.string/includes? (bytes-of wrapped) wrapper-keyword-hex)
          "the wrapper keyword is in the block"))
    (testing "after"
      (let [u (unwrap wrapped)]
        (is (= value/code-float (first (form u))))
        (is (not (clojure.string/includes? (bytes-of u) wrapper-keyword-hex))
            "and it is gone")
        (is (= 1.5 (value/float64-value (value/form->value (form u))))
            "carrying the value it always meant")))))

(deftest an-out-of-range-i64-reaches-the-exact-integer-only-after-unwrapping
  (let [wrapped (identity/i64 9223372036854775807)]
    (testing "before"
      (is (= value/code-map (first (form wrapped))))
      (is (clojure.string/includes? (bytes-of wrapped) wrapper-keyword-hex)))
    (testing "after"
      (let [u (unwrap wrapped)]
        (is (= value/code-int64 (first (form u))))
        (is (not (clojure.string/includes? (bytes-of u) wrapper-keyword-hex)))
        (is (= 9223372036854775807 (value/int64-value (value/form->value (form u))))
            "exactly, which is the whole reason the wrapper existed")))))

(deftest unwrap-touches-nothing-else
  ;; The conversion has to be narrow. A postwalk that rewrote more than the two
  ;; wrappers would move values that already encode correctly, and every one of
  ;; those is a DefCID that did not need to move.
  (doseq [v [nil true 5 "a" :a :host/http 'a 'ns/a [1 2] '(1 2) #{1 2} {:a 1}
             {:a {:b [1 #{:c}]}}]]
    (is (= v (unwrap v)) (str "unwrap changed " (pr-str v))))
  ;; and a one-entry map that merely LOOKS like a wrapper but is not
  (is (= {:kotoba.lang.code-identity/f64 42} (unwrap {:kotoba.lang.code-identity/f64 42}))
      "the wrapper predicate requires a string payload; a non-string is an ordinary map"))

(deftest every-frozen-vector-payload-converts-and-nothing-carries-the-wrapper
  (is (= 10 (count vectors))
      "the frozen vectors did not load; every assertion below would be vacuous")
  (let [n (atom 0)]
    (doseq [{:keys [id definition]} vectors]
      (testing (str id)
        (let [payload (identity/identity-payload definition)
              u (unwrap payload)]
          (is (some? (form u)) (str id ": the unwrapped payload did not convert"))
          (is (not (clojure.string/includes? (bytes-of u) wrapper-keyword-hex))
              (str id ": a wrapper keyword survived into the block"))
          (swap! n inc))))
    (is (= 10 @n))
    (println (str "CONVERTED\t" @n "\tfrozen payloads"))))

(deftest the-conversion-is-not-identity
  ;; The discriminating control. Every assertion above would also hold if the
  ;; wrappers had never been a problem, so one case has to show the difference
  ;; the conversion makes on a payload that actually contains one.
  (let [definition #:definition{:profile-version 4
                                :desugar-contract-version 1
                                :kir {:op :const :value (identity/f64 1.5)}
                                :effect-row #{}
                                :interface {:arity 0 :result :f64}
                                :dependencies []}
        payload (identity/identity-payload definition)]
    (is (clojure.string/includes? (bytes-of payload) wrapper-keyword-hex)
        "this payload does contain a wrapper, so the case can discriminate")
    (is (not (clojure.string/includes? (bytes-of (unwrap payload)) wrapper-keyword-hex)))
    (is (not= (bytes-of payload) (bytes-of (unwrap payload)))
        "and the two encodings differ, which is what payload v3 would be buying")))
