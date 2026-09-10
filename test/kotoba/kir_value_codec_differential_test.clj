(ns kotoba.kir-value-codec-differential-test
  "How far the definition identity's value model is from `kotoba.value.v1`.

  Root ADR `adr-2609076000-kir-is-an-ipld-codec-schema-and-adl` decided that
  this repository's canonical bytes should come from `io-ipld` rather than from
  a second normalization here, and named ONE measurement as the next step:
  for each frozen vector, the bytes `canonical-bytes` produces today against
  the bytes the same logical payload produces through `kotoba.value.codec`.
  It decides the only thing that sets the migration's cost -- whether payload
  v3 is needed at all.

  ## The answer, and why it is asserted rather than described

  It is needed. All ten frozen vectors disagree, so adopting the value model
  would move every DefCID, every module lock and every compile cache key.

  That fact is pinned in BOTH directions here, which is unusual for a test and
  deliberate. The agreement half (`canonical-bytes` still reproduces every
  frozen hex) is an ordinary golden. The disagreement half is a **debt
  marker**: it fails when someone closes the gap, because closing it silently
  is exactly the failure this repository's own history warns about -- payload
  v1 addressed `pr-str` output under a dag-cbor label, and nothing said so
  until someone measured. A test that only asserted the goldens would stay
  green through a change that moved every identity in the workspace.

  ## What is NOT divergent

  The codec is already shared. Both encoders are `(cbor/encode <tagged form>)`
  over the same `cbor.core` from `org-ietf-cbor`, and both address the result
  as CIDv1. There is no second DAG-CBOR encoder in this repository, and the
  ADR's phrase \"a second normalization\" should be read as the VALUE MODEL
  above the codec, not the codec.

  ## The three dimensions

  Measured, not inferred -- `value-models-differ-in-three-dimensions` shows
  both forms for one sample:

    normalize     [\"map\" [[[\"kw\" \"n\"] [\"int\" \"1\"]] ...]]
    value->form   [19    [[[5    \"n\"] [2     1  ]] ...]]

  1. **Tag alphabet** -- strings here, integers there.
  2. **Scalar payload** -- integers as exact decimal TEXT here (so `:clj` and
     `:cljs` agree on a 64-bit literal), native integers there (with an
     append-only scalar code for exact i64).
  3. **Map key order** -- `value->form` uses DAG-CBOR's canonical length-first
     ordering; `normalize` uses its own `rank`/`cmp`. Neither is wrong:
     `normalize` never emits a CBOR map at all, it emits `[\"map\" [pairs]]`,
     so the codec's map-ordering rule does not bind it and it had to pick one.

  A migration that changed only the tag alphabet would still move every CID
  through dimensions 2 and 3."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [cbor.core :as cbor]
            [ipld.value :as value]
            [kotoba.kir.definition-identity :as identity]))

(def ^:private vectors
  (-> "kotoba/kir/fixtures/code-identity-vectors.edn"
      io/resource
      (or (io/file "test/kotoba/kir/fixtures/code-identity-vectors.edn"))
      slurp
      edn/read-string
      :vectors))

(def ^:private expected-vector-count
  "A floor, so a fixture that failed to load cannot report a clean run.
   `SCANNED 0` is not `SCANNED 10`."
  10)

(defn- hex [^bytes bs]
  (apply str (map #(format "%02x" (bit-and % 0xff)) (seq bs))))

(deftest the-fixture-actually-loaded
  (is (= expected-vector-count (count vectors))
      "the frozen vectors did not load; every assertion below would be vacuous"))

(deftest canonical-bytes-still-reproduce-every-frozen-vector
  (testing "the agreement half: this repository's encoder has not moved"
    (doseq [{:keys [id definition canonical-hex definition-cid]} vectors]
      (is (= canonical-hex (hex (identity/canonical-bytes definition)))
          (str id " canonical bytes moved"))
      (is (= definition-cid (identity/definition-cid definition))
          (str id " definition CID moved")))))

(deftest every-frozen-vector-disagrees-with-the-value-codec
  (testing "the debt marker: adopting kotoba.value.v1 would move every DefCID"
    (let [disagreeing
          (doall
           (for [{:keys [id definition]} vectors
                 :let [ours (hex (identity/canonical-bytes definition))
                       theirs (hex (value/encode-value
                                    (identity/identity-payload definition)))]
                 :when (not= ours theirs)]
             id))]
      (is (= expected-vector-count (count disagreeing))
          (str "The measured gap changed. If FEWER vectors disagree, someone "
               "moved one of the two value models and every DefCID that "
               "vector stands for moved with it -- that is payload v3 and it "
               "is a decision, not a refactor. Agreeing vectors: "
               (pr-str (remove (set disagreeing) (map :id vectors))))))))

(deftest the-value-codec-accepts-the-payload-it-disagrees-about
  (testing "the disagreement is the value model, not a refusal"
    (doseq [{:keys [id definition]} vectors]
      (is (bytes? (value/encode-value (identity/identity-payload definition)))
          (str id ": kotoba.value.v1 refused the payload. Then the vectors "
               "above disagree for a second reason and the count assertion "
               "is measuring the wrong thing.")))))

(deftest value-models-differ-in-three-dimensions
  (testing "named, so a migration cannot be scoped as a tag rename"
    (let [sample {:effect-row #{:host/http} :n 1 :s "a"}
          ours (identity/normalize sample)
          theirs (value/value->form sample)]
      (is (= ["map" [[["kw" "effect-row"] ["set" [["kw" "host/http"]]]]
                     [["kw" "n"] ["int" "1"]]
                     [["kw" "s"] ["str" "a"]]]]
             ours)
          "normalize: string tags, integers as decimal text, its own key order")
      (is (= [19 [[[5 "n"] [2 1]]
                  [[5 "s"] [4 "a"]]
                  [[5 "effect-row"] [18 [[5 "host/http"]]]]]]
             theirs)
          "value->form: integer tags, native integers, DAG-CBOR length-first key order")
      (is (not= (first ours) (first theirs)) "dimension 1: tag alphabet")
      (is (not= (get-in ours [1 1 1]) (get-in theirs [1 0 1]))
          "dimension 2: scalar payload representation")
      (is (not= (mapv (comp second first) (second ours))
                (mapv (comp second first) (second theirs)))
          "dimension 3: map key order"))))

(deftest the-codec-underneath-is-already-shared
  (testing "there is no second DAG-CBOR encoder to unify"
    (let [form ["map" [[["kw" "a"] ["int" "1"]]]]]
      (is (= (hex (cbor/encode form))
             (hex (cbor/encode form)))
          "cbor.core is deterministic")
      (is (= (hex (identity/canonical-bytes (:definition (first vectors))))
             (hex (cbor/encode
                   (identity/normalize
                    (identity/identity-payload (:definition (first vectors)))))))
          (str "canonical-bytes is exactly (cbor/encode (normalize payload)) "
               "over the same cbor.core io-ipld uses; the divergence is above "
               "the codec, not in it")))))

;; ---------------------------------------------------------------------------
;; What a naive payload v3 would do, measured
;; ---------------------------------------------------------------------------

(deftest a-naive-payload-v3-encodes-every-float-and-large-integer-as-a-map
  ;; The three dimensions above are byte-level: a migration pays them once and
  ;; every CID moves, which is the cost already on the record. This is a
  ;; DIFFERENT thing and it is not a cost, it is a defect waiting for whoever
  ;; performs the swap.
  ;;
  ;; `normalize` carries an f64 as `{::f64 "<16 hex>"}` and an out-of-range i64
  ;; as `{::i64 "<decimal>"}` -- ordinary Clojure MAPS whose keys this
  ;; namespace's identity docstring says must keep the pre-extraction
  ;; namespace, because changing them "would reinterpret old frozen definitions
  ;; as ordinary maps and therefore move their DefCIDs".
  ;;
  ;; `value->form` does not know those wrappers. Measured 2026-09-10, it does
  ;; not refuse them either: it accepts each one as what it structurally is, a
  ;; one-entry map, and encodes the wrapper KEYWORD into the block. So a v3
  ;; that hands `identity-payload` straight to the value model produces a
  ;; block that encodes, round-trips, and has quietly replaced every float with
  ;; a map containing a magic keyword -- the accepted-but-wrong case, not the
  ;; refused one.
  ;;
  ;; The work item this names: v3 must unwrap into `value/float64` and
  ;; `value/int64` BEFORE encoding. It is one conversion, but nothing fails if
  ;; it is forgotten.
  (testing "an f64 literal"
    (let [wrapper (identity/f64 1.5)
          naive (value/value->form wrapper)
          native (value/value->form (value/float64 1.5))]
      (is (= ["f64" "3ff8000000000000"] (identity/normalize wrapper))
          "normalize tags it as a float")
      (is (= value/code-map (first naive))
          "value->form did not refuse the wrapper; it took it for a map")
      (is (= value/code-float (first native))
          "the value model does have a float; the wrapper simply never reaches it")
      (is (not= (hex (cbor/encode naive)) (hex (cbor/encode native)))
          "so the two are different blocks for one value")
      (is (clojure.string/includes?
           (hex (cbor/encode naive))
           (hex (.getBytes "kotoba.lang.code-identity/f64" "UTF-8")))
          "and the wrapper keyword is inside the block a DefCID would address")))

  (testing "an i64 outside the exactly-representable range"
    (let [wrapper (identity/i64 9223372036854775807)
          naive (value/value->form wrapper)]
      (is (= ["int" "9223372036854775807"] (identity/normalize wrapper))
          "normalize tags it as an integer")
      (is (= value/code-map (first naive))
          "value->form took it for a map here too")
      (is (= value/code-int64 (first (value/value->form (value/int64 9223372036854775807))))
          "the value model has an exact i64; the wrapper never reaches it")))

  (testing "and the encoder is not simply accepting everything"
    ;; The discriminating control. If `value->form` admitted every input, the
    ;; assertions above would say nothing about wrappers in particular.
    (is (= :value/integer-out-of-range
           (try (value/value->form 9223372036854775807)
                nil
                (catch clojure.lang.ExceptionInfo e (:problem (ex-data e)))))
        "a raw out-of-range integer IS refused, so acceptance above is a fact about the wrapper")))
