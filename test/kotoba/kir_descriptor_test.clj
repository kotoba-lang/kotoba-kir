(ns kotoba.kir-descriptor-test
  "The descriptor encoding, now that it has two consumers rather than one.

  While it lived in `kotoba.wasm.typed` it was checked only through the wasm
  custom section it fed, so its bytes were pinned indirectly. It is about to
  be handed to a second reader -- the native loader, which today receives an
  integer `kind` and therefore cannot carry a capability whose request is a
  variant or a record. Two readers make the encoding a wire contract, and a
  wire contract needs its own test: if the encoder and a future decoder are
  only ever checked against each other, they can agree on the same mistake."
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.kir.descriptor :as descriptor]))

;; ---------------------------------------------------------------------------
;; Primitives and aliases
;; ---------------------------------------------------------------------------

(deftest a-primitive-is-its-tag-byte
  (doseq [[tag byte] {:i64 0 :string 1 :keyword 2 :bool 3 :f64 12 :f32 13
                      :vector-i64 11 :vector-f64 14 :document 18 :bytes 21}]
    (testing (str tag)
      (is (= [byte] (descriptor/encode-descriptor tag))))))

;; The two scalar ADT aliases are spellings, not types: they must encode
;; identically to what they expand to, or a producer using the short spelling
;; and one using the long spelling would disagree about the same value.
(deftest a-scalar-adt-alias-encodes-as-its-expansion
  (is (= (descriptor/encode-descriptor [:option :i64])
         (descriptor/encode-descriptor :option-i64)))
  (is (= (descriptor/encode-descriptor [:result :i64 :i64])
         (descriptor/encode-descriptor :result-i64))))

;; ---------------------------------------------------------------------------
;; Structures
;; ---------------------------------------------------------------------------

(deftest a-structure-is-its-tag-then-its-members
  (is (= [4 0] (descriptor/encode-descriptor [:option :i64])))
  (is (= [5 0 1] (descriptor/encode-descriptor [:result :i64 :string])))
  (is (= [8 0] (descriptor/encode-descriptor [:set :i64])))
  (is (= [20 1] (descriptor/encode-descriptor [:list :string])))
  (is (= [10 1 0] (descriptor/encode-descriptor [:map :string :i64])))
  (is (= [7 2 0 1] (descriptor/encode-descriptor [:vector [:i64 :string]]))
      "a heterogeneous vector carries its arity before its members"))

;; The shape the native capability boundary cannot carry today, and the reason
;; this namespace moved: `clock-v1`'s request is a variant, and its result is a
;; variant whose payload is a record. Both encode here without any special
;; case -- the limit is in the boundary, not in the descriptor language.
(deftest the-shapes-the-native-boundary-cannot-yet-carry-encode-fine
  (is (seq (descriptor/encode-descriptor
            [:variant :kotoba.clock/request [[:wall :bool] [:monotonic :bool]]])))
  (is (seq (descriptor/encode-descriptor
            [:variant :kotoba.clock/result
             [[:ok [:record :kotoba.clock/reading [[:seconds :i64] [:nanos :i64]]]]
              [:denied :string]]]))))

(deftest a-name-is-carried-so-two-shapes-with-different-names-differ
  ;; Structural equality is not enough for a nominal type: two records with the
  ;; same fields and different names are different types, and a boundary that
  ;; erased the name would accept one where the other was meant.
  (is (not= (descriptor/encode-descriptor [:record :a/thing [[:x :i64]]])
            (descriptor/encode-descriptor [:record :b/thing [[:x :i64]]])))
  (is (not= (descriptor/encode-descriptor [:variant :a/tag [[:x :i64]]])
            (descriptor/encode-descriptor [:variant :b/tag [[:x :i64]]]))))

(deftest field-order-is-carried
  ;; Records are positional once encoded, so a reader walking a value against a
  ;; descriptor must see the same order the writer used.
  (is (not= (descriptor/encode-descriptor [:record :a/r [[:x :i64] [:y :string]]])
            (descriptor/encode-descriptor [:record :a/r [[:y :string] [:x :i64]]]))))

(deftest an-unknown-descriptor-is-refused-rather-than-encoded-as-something-else
  (is (thrown? clojure.lang.ExceptionInfo
               (descriptor/encode-descriptor [:tuple :i64 :i64]))
      "an unknown STRUCTURE is named in a phase-tagged rejection")
  ;; An unknown bare keyword is refused too, but by falling into `first` rather
  ;; than by reaching the explicit rejection below it, so it arrives as an
  ;; IllegalArgumentException with no phase and no descriptor in its data.
  ;; Pinned as measured, not as preferred: this namespace was extracted without
  ;; behaviour change, and tightening the diagnostic here would make the move a
  ;; behaviour change wearing a refactor's commit message. It matters more now
  ;; that a second reader is coming, so it is worth fixing -- separately.
  (is (thrown? IllegalArgumentException
               (descriptor/encode-descriptor :not-a-type))))

;; ---------------------------------------------------------------------------
;; Walking KIR
;; ---------------------------------------------------------------------------

(defn- kir [body]
  {:format :kotoba.kir/v4 :entry 'main :exports ['main]
   :functions [{:name 'main :params [] :result :i64 :body body}]})

(deftest the-table-is-sorted-so-two-builds-agree
  ;; Indices into this table are what a module actually carries, so an
  ;; unstable order would make two builds of the same source disagree about
  ;; which descriptor a given index means.
  (let [a (descriptor/descriptor-table (kir '(string-concat "x" "y")))
        b (descriptor/descriptor-table (kir '(string-concat "y" "x")))]
    (is (= a b))
    (is (= a (vec (sort-by pr-str a))))))

(deftest a-capability-contract-is-collected-per-id
  (let [contracts (descriptor/capability-contracts
                   (kir '(typed-cap-call 7 :string :string "x")))]
    (is (= 1 (count contracts)))
    (is (= 7 (:id (first contracts))))
    (is (= :string (:request-type (first contracts))))))

(deftest one-capability-id-may-not-carry-two-contracts
  ;; The invariant that lets a module name a capability by id alone.
  (is (thrown? clojure.lang.ExceptionInfo
               (descriptor/capability-contracts
                (kir '(+ (typed-cap-call 7 :string :string "x")
                         (typed-cap-call 7 :i64 :i64 1)))))))
