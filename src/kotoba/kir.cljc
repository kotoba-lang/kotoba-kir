(ns kotoba.kir
  ;; The whole `:require` clause (not just an item inside it) is behind the
  ;; reader-conditional: on `:clj` this file needs no extra require at all
  ;; (matching the original `(ns kotoba.kir)`), and an EMPTY
  ;; `(:require)` clause -- which is what results if only an item inside it
  ;; is conditional and the branch doesn't match -- fails ns-form spec
  ;; validation ("Extra input spec: :clojure.core.specs.alpha/ns-form",
  ;; confirmed live).
  (:require [clojure.string :as str]
            [kotoba.hir :as hir]
            [kotoba.kir.value :as value]
            [kotoba.kir.decimal :as decimal]
            [kotoba.kir.xml :as xml]
            #?@(:cljs [[kotoba.kir.cljs-i64 :as i64]])))

(def ^:private default-fuel 512)
;; Compile-time constant oracle may need more budget than the historical
;; runtime default (T7.2 / T7.4 deep loop). Runtime `execute` still defaults
;; to `default-fuel` (512) unless the caller passes `:fuel`.
(def ^:private oracle-fuel 100000)
(def ^:private default-pair-capacity 4096)
(def ^:private default-kgraph-capacity 4096)
(def ^:dynamic *runtime-schemas* nil)

;; Shared between `kotoba.compiler.core` (JVM `clojure -M:run` path) and
;; `kotoba.compiler.nbb.cli` (nbb-native fast path) -- both admit
;; `:kotoba.hir/v3` (typed) HIR onto the x86_64/aarch64 native backends
;; ONLY when the actual features used are limited to string literals +
;; `string-byte-length`/`string=?`/`string-concat` (the pre-existing typed
;; native slice) PLUS -- as of the first native record increment -- a
;; SEALED, ALL-SCALAR (`:i64`/`:bool` fields only, no `:f64`: see
;; `native-word-field-types`'s own comment for why f64 is
;; deliberately excluded even though it's part of the WASM-track ADR 0043
;; this slice models) `record-new`/`record-get` pair used in the one exact
;; nested shape `backend/x86-64.cljc`'s and `backend/aarch64.cljc`'s own
;; `emit-record-get-of-new` implement: `record-get`'s value operand must be
;; a directly-nested, SAME-schema `record-new`, plus monomorphic
;; `:option-i64`/`:result-i64` constructors, projections, and same-type
;; capability callbacks using canonical pair-backed `(tag,payload)` handles.
;; Every other typed feature (maps, generic options/results, variants,
;; general/escaping/nested records, typed sets, heterogeneous vectors, ...)
;; still requires the kotoba-script web target or typed Wasm target. A blanket
;; per-backend allowance would
;; silently let unsupported ops reach the backend and crash confusingly
;; instead of rejecting cleanly -- so admission has to inspect which
;; features are actually used, not just the HIR format tag.
(def non-string-typed-ops
  '#{string-replace-all string-contains? string-split-count string-fold-case
     f32-to-bits f32-from-bits f64-to-f32-rounded f32-to-f64-exact
     f32-add f32-sub f32-mul f32-div f32-min f32-max f32-neg f32-abs f32-sqrt
     f32-eq f32-lt f32-le f32-gt f32-ge f32-unordered
     i64-to-f32-checked i64-to-f32-rounded f32-to-i64-checked f32-to-i64-truncating
     f64-to-bits f64-from-bits f64-add f64-sub f64-mul f64-div f64-min f64-max
     f64-neg f64-abs f64-sqrt f64-sin-quarter-turn f64-cos-quarter-turn
     f64-sin-bounded f64-cos-bounded f64-exp-near-zero f64-log-near-one f64-atan2-bounded
     f64-exp-bounded f64-log-bounded
     f64-eq f64-lt f64-le f64-gt f64-ge f64-unordered
     i64-to-f64-checked i64-to-f64-rounded f64-to-i64-checked f64-to-i64-truncating
     map-new map-get map-assoc
     bool-not option-some option-none option-some? option-value
     result-ok result-err result-ok? result-value result-error
     result-ok-of result-err-of result-ok?-of result-value-of result-error-of result-match-of
     variant-new variant-match
     option-some-of option-none-of option-some?-of option-value-of option-match
     typed-list-new bytes-empty
     hetero-vector-new hetero-vector-count hetero-vector-at hetero-vector-assoc hetero-vector-equal
     typed-set-new typed-set-count typed-set-contains typed-set-conj typed-set-disj typed-set-equal typed-set-nth
     typed-map-new typed-map-count typed-map-contains typed-map-get
     typed-map-entry-at typed-map-assoc typed-map-dissoc typed-map-equal
     xml-path-count xml-name-count xml-name-text xml-path-text xml-path-attr
     decimal-f64-parse decimal-f64x3-parse
     record-new record-get record-assoc record-equal
     vector-count vector-get vector-at vector-drop vector-assoc vector-assoc! vector-conj vector-alloc
     vector-f64-new vector-f64-count vector-f64-get vector-f64-at
     vector-f64-drop vector-f64-assoc vector-f64-conj
     string-index-new string-index-count string-index-contains string-index-get string-index-assoc
     disjoint-set-i64-new disjoint-set-i64-count disjoint-set-i64-union
     document-null document-bool document-i64 document-f64 document-string document-keyword document-symbol
     document-vector document-list document-set document-map document-count document-kind document-equal? document-set-contains? document-contains document-get
     document-vector-at document-list-at document-map-entry-at document-vector-assoc document-vector-conj document-vector-drop
     document-vector-remove
     document-assoc document-dissoc document-merge document-string-value document-keyword-value document-symbol-value
     document-bool-value document-i64-value document-f64-value document-sha256 document-print document-read
     document-edn-print document-edn-read
     i32-wrap u32-wrap i32-wrapping-add i32-wrapping-mul i32-xor
     i32-shift-left i32-shift-right u32-shift-right xorshift32
     bit-or bit-not i64-shift-left i64-shift-right u64-shift-right
     keyword-from-string keyword-name symbol})

;; The two field kinds this backend's own runtime value representation is
;; ALREADY bit-identical for: every existing x86-64.cljc/aarch64.cljc
;; comparison/setcc sequence already carries `:bool` as a plain 0/1 word
;; (see e.g. x86-64.cljc's `contains? '#{= < > <= >=}` case), and `:i64`
;; needs no conversion at all -- there is no narrower-than-8-bytes packing
;; anywhere else in either file. `:f64` is deliberately NOT admitted here
;; even though it's part of ADR 0043 (the WASM Component Model analog this
;; native slice is modeled on). When this comment was written the reason was
;; that `kotoba.compiler.core/compile-source*`'s own gate rejected ANY
;; `:f32`/`:f64` usage on native (`ir/uses-f32?`/`ir/uses-f64?`); both widths
;; now reach native as OPERATIONS over i64 words, so the remaining reason is
;; narrower and is about the FIELD SLOT, not the gate: a record field holding a
;; float would have to declare which of the two widths its 8 bytes carry, and
;; nothing in the slot machinery reads a field's declared type at runtime.
;; Admitting one here would silently also have to widen that orthogonal,
;; still-separate question, which is exactly the "don't
;; widen two dimensions in one step" pattern this compiler's own component
;; ADR chain (0058/0059) explicitly avoids. Native f64 record fields remain
;; a separately-gapped follow-up, not attempted by this increment.
;; Renamed from `native-scalar-record-field-types`: `:string` is not scalar, and
;; the set is no longer about scalars but about what fits in ONE WORD. The
;; surrounding `native-scalar-record-type?` / `native-scalar-variant-type?`
;; names are left as they are only because they are quoted by comments in
;; kotoba-native and kotoba-verifier.
;;
;; `:string` is admissible because a string value on native already IS a
;; one-word `pair(offset,length)` handle -- the same width as an `:i64` -- so a
;; field or payload holding one needs no representation the slot machinery does
;; not already have. Verified by handing both backends a KIR with string fields
;; and string variant payloads directly, bypassing this gate: all four cases
;; emit, including projecting the string back out and measuring it.
;;
;; `:f64` stays out for the reason the previous comment gave, restated: the
;; question is not whether native can compute on floats -- it can, at both
;; widths -- but that a float field would have to say which width its one word
;; carries, and no field's declared type is read at runtime. That is a separate
;; question from the operation gate and is not answered here.
;; `:keyword` is admitted for the same reason `:string` is:
;; the backends now carry a keyword as the same one-word pair(offset,length)
;; handle, over its printed text, which is the representation
;; `kotoba.wasm.core` already chose for keyword literals. That admitted keyword
;; VALUES and not keyword OPERATIONS for as long as `keyword-name` and
;; `keyword-from-string` wanted a general substring and concatenation over a
;; runtime handle; both landed 2026-08-04, so both operations are admitted
;; below and each backend desugars into exactly them.
(def ^:private native-word-field-types #{:i64 :bool :string :keyword :document})

;; Structural shape check only (`[:record :qualified/kw [[:field :type] ...]]`)
;; -- deliberately does not re-derive `kotoba.compiler.frontend`'s own
;; `record-type?`/`validate-value-type!` (that generic check already ran
;; before `ir/lower` produced this HIR), just narrows it further to the
;; scalar-only field-type universe this native increment implements.
(declare native-scalar-record-type?)
(declare native-word-value-type?)

(defn- native-scalar-record-type? [type]
  (and (vector? type) (= 3 (count type)) (= :record (first type))
       (keyword? (second type)) (some? (namespace (second type)))
       (vector? (nth type 2)) (seq (nth type 2))
       ;; A field may also be a record, which the backends FLATTEN into the
       ;; enclosing record's own slots -- recursively -- so a nested record
       ;; still never needs a runtime representation of its own. The criterion
       ;; here is therefore no longer only fits-in-one-word but
       ;; flattens-into-words.
       (every? (fn [field]
                 (and (vector? field) (= 2 (count field)) (keyword? (first field))
                      (or (contains? native-word-field-types (second field))
                          ;; An `[:option T]`/`[:result T E]` field is admitted
                          ;; for exactly the reason the word types above are: it
                          ;; already travels as ONE word on both backends (the
                          ;; pair handle `option-some-of`/`result-ok-of` build),
                          ;; so it occupies a single flattened slot like any
                          ;; other field and needs no representation the record
                          ;; lowering does not already have. Without it a schema
                          ;; as ordinary as `[[:mem [:option :i64]] [:tmax :i64]]`
                          ;; -- murakumo's own `:join/clamp` -- was unrepresentable
                          ;; while each of its parts was representable alone.
                          (native-word-value-type? (second field))
                          (native-scalar-record-type? (second field)))))
               (nth type 2))
       (= (count (nth type 2)) (count (distinct (map first (nth type 2)))))))

;; ADR 0063: the second native value-representation increment (right after
;; ADR 0062's record). A native sealed variant admits the SAME narrow
;; per-case payload universe records already admit (`:i64`/`:bool` only, no
;; `:f64` -- identical reasoning as `native-word-field-types`'s own
;; comment: native f32/f64 is a separate, orthogonal, pre-existing gate this
;; increment does not touch). A "tag-only" (unit-like) case is realized
;; WITHOUT any new type-system concept: every case still declares a real
;; `:i64`/`:bool` payload type and every `variant-new` call still supplies a
;; real payload expression (this frontend's shared, target-independent
;; `variant-new`/`variant-match` grammar, unchanged by this ADR, always
;; requires exactly one payload per case) -- a case is "tag-only" purely by
;; the codegen/test convention of never binding or reading that case's own
;; payload word in its branch body, matching this backend's own "every
;; value, including an unused one, is still a uniform 8-byte word" stance
;; already established by ADR 0062. See docs/adr/0063-* for why this ADR
;; deliberately does NOT introduce a genuine zero-payload marker type.
(defn- native-scalar-variant-type? [type]
  (and (vector? type) (= 3 (count type)) (= :variant (first type))
       (keyword? (second type)) (some? (namespace (second type)))
       (vector? (nth type 2)) (seq (nth type 2))
       (every? (fn [case-entry]
                 (and (vector? case-entry) (= 2 (count case-entry)) (keyword? (first case-entry))
                      ;; A case payload may itself be a record: the backends
                      ;; flatten it into the dispatch's own payload slots, and
                      ;; size that region by the widest declared case.
                      (or (contains? native-word-field-types (second case-entry))
                          (native-scalar-record-type? (second case-entry)))))
               (nth type 2))
       (= (count (nth type 2)) (count (distinct (map first (nth type 2)))))))

(defn- native-boundary-scalar-variant-type? [type]
  ;; The public kexe boundary owns a deliberately smaller representation than
  ;; the expression evaluator: declaration ordinal + one scalar payload in a
  ;; context-owned pair handle. Nested record payloads remain local-only.
  (and (vector? type) (= 3 (count type)) (= :variant (first type))
       (keyword? (second type)) (some? (namespace (second type)))
       (vector? (nth type 2)) (<= 1 (count (nth type 2)) 32)
       (every? (fn [case-entry]
                 (and (vector? case-entry) (= 2 (count case-entry))
                      (keyword? (first case-entry))
                      (contains? #{:i64 :bool} (second case-entry))))
               (nth type 2))
       (= (count (nth type 2))
          (count (distinct (map first (nth type 2)))))))

(def ^:private native-clock-request-type
  [:variant :kotoba.clock/request [[:wall :bool] [:monotonic :bool]]])

(def ^:private native-clock-result-type
  [:variant :kotoba.clock/result
   [[:wall [:record :kotoba.clock/wall
            [[:unix-millis :i64] [:observation-sequence :i64]]]]
    [:monotonic [:record :kotoba.clock/monotonic
                 [[:nanos :i64] [:observation-sequence :i64]]]]
    [:error [:record :kotoba.clock/error
             [[:code :keyword] [:message :string]]]]]])

(def ^:private native-dataspace-request-type
  [:variant :kotoba.dataspace/request
   [[:assert [:record :kotoba.dataspace/assert
              [[:assertion :document] [:facet :i64]]]]
    [:retract [:record :kotoba.dataspace/retract
               [[:assertion :document] [:facet :i64]]]]
    [:observe [:record :kotoba.dataspace/observe
               [[:pattern :document] [:facet :i64]]]]
    [:facet-enter :bool]
    [:facet-leave :i64]]])

(def ^:private native-dataspace-result-type
  [:variant :kotoba.dataspace/result
   [[:asserted [:record :kotoba.dataspace/asserted
                [[:count :i64] [:notices :document]]]]
    [:retracted [:record :kotoba.dataspace/retracted [[:count :i64]]]]
    [:matches [:record :kotoba.dataspace/matches
               [[:bindings :document] [:notices :document]]]]
    [:facet [:record :kotoba.dataspace/facet [[:id :i64]]]]
    [:error [:record :kotoba.dataspace/error
             [[:code :keyword] [:message :string]]]]]])

(def ^:private native-ui-parent-type [:option :keyword])
(def ^:private native-ui-node-type
  [:record :kotoba.ui/node
   [[:id :keyword] [:parent native-ui-parent-type]
    [:kind :keyword] [:text :string]]])
(def ^:private native-ui-node-set-type [:set native-ui-node-type])
(def ^:private native-ui-commit-request-type
  [:record :kotoba.ui/commit-request
   [[:base-revision :i64] [:nodes native-ui-node-set-type]]])
(def ^:private native-ui-commit-result-type
  [:record :kotoba.ui/commit-result [[:revision :i64] [:node-count :i64]]])
(def ^:private native-ui-event-request-type
  [:record :kotoba.ui/event-request [[:after-revision :i64]]])
(def ^:private native-ui-event-type
  [:record :kotoba.ui/event
   [[:revision :i64] [:target :keyword] [:kind :keyword] [:value :string]]])
(def ^:private native-ui-event-result-type [:option native-ui-event-type])

(defn- native-provider-contract? [cap-id request-type result-type]
  (or (and (= 7 cap-id)
           (= native-clock-request-type request-type)
           (= native-clock-result-type result-type))
      (and (= 24 cap-id)
           (= native-dataspace-request-type request-type)
           (= native-dataspace-result-type result-type))
      (and (= 9 cap-id)
           (= native-ui-commit-request-type request-type)
           (= native-ui-commit-result-type result-type))
      (and (= 10 cap-id)
           (= native-ui-event-request-type request-type)
           (= native-ui-event-result-type result-type))))

(defn- native-word-value-type?
  "Types whose runtime value fits the native backend's uniform 64-bit word.
  Structured option/result values are pair handles, so they compose
  recursively without changing the machine ABI.
  `:document` is the same width as `:string`: a pair(offset,length) over
  canonical UTF-8 EDN bytes. That is a backend representation, not the
  application programming model."
  ([type] (native-word-value-type? type 0))
  ([type depth]
   (and (<= depth 8)
        (or (contains? #{:i64 :bool :string :keyword :document
                         :option-i64 :result-i64} type)
            (and (vector? type)
                 (case (first type)
                   :option (and (= 2 (count type))
                                (or (native-word-value-type? (second type) (inc depth))
                                    (native-scalar-record-type? (second type))))
                   :set (and (= 2 (count type))
                             (or (native-word-value-type? (second type) (inc depth))
                                 (native-scalar-record-type? (second type))))
                   :result (and (= 3 (count type))
                                (native-word-value-type? (second type) (inc depth))
                                (native-word-value-type? (nth type 2) (inc depth)))
                   false))))))

;; A function-boundary type the native backends can carry in ONE machine word,
;; or box into one.
;;
;; `param-types` and `result` are the only place a `[:ref :ns/name]` survives
;; lowering -- every `record-get` in a body already carries its schema expanded
;; (confirmed by reading a lowered body: `(record-get [:record :join/relay …] r
;; :tier)`), so expansion is needed here and nowhere else.
;;
;; Records are admitted in BOTH directions now, not just as results. A record
;; parameter travels exactly the way a record result already did -- boxed into
;; the `pair` chain both backends have built since ADR 0062 -- so this adds no
;; representation, no ABI change and no new primitive. It was the single thing
;; standing between this backend and the pure planner cores real callers
;; already ship: measured 2026-08-05, every one of murakumo's 33 `*_core.kotoba`
;; modules that failed here failed on a record parameter, an `[:option T]`
;; record field, or a `:bool` result -- never on anything in a body.
;;
;; A bare `:bool` PARAMETER is now admitted too, and the reason it was excluded
;; needs correcting rather than deleting, because the exclusion rested on a
;; misread of its own measurement.
;;
;; The native-record-parameters decision (this repo's ADR 0221 supersedes that
;; part of it; upstream it is `compiler` ADR 0219 / superproject
;; ADR-2608052000) recorded: "`execute` validates a `:bool` argument as an i64",
;; citing `{:trap :value-type-mismatch :expected :i64 :position {:parameter b}}`.
;; The trap is real and still reproduces (`kir-bool-parameter-test` pins it),
;; but it is not the interpreter refusing a `:bool` parameter. It is the
;; interpreter CORRECTLY refusing a host boolean where the KIR it was handed
;; DECLARES `:i64` -- because that KIR carries no `:param-types` table at all,
;; and `invoke-function` defaults an absent one to `:i64` per parameter.
;;
;; The table is absent because `lower` keeps `:param-types` only for
;; `:kotoba.hir/v3`, and `kotoba.compiler.frontend`'s `typed-values?` excludes
;; `:bool` by name ("`:bool` literals are plain 0/1 words, not typed values").
;; So a module whose ONLY typed feature is a `:bool` parameter is emitted as
;; `:kotoba.hir/v2` and loses its parameter types on the way down. That is a
;; live gap, but it lives in the compiler's format classification, not here,
;; and closing it moves `:kir-sha256` for every affected module on every
;; target -- so it is named as a follow-on, not done in passing.
;;
;; Where the table IS present -- any module with a `:bool` parameter alongside
;; any other typed feature, which is exactly the shape this gate governs --
;; `execute` runs a `:bool` parameter today and always could. Measured
;; 2026-08-05 and pinned by `kir-bool-parameter-test`: a host boolean crossing
;; the entry boundary, an `if` test, `bool-not`, `=`, a `let` rebinding, a call
;; into another `:bool` parameter, a `:bool` record field built from it, and a
;; `:bool` result boxed back out. The boundary convention is unchanged and now
;; symmetric in both directions: a host boolean is what ENTERS (the argument
;; check below requires `boolean?` and rejects the word `1`) and a host boolean
;; is what LEAVES (`box-bool`), while inside a module `:bool` stays a plain 0/1
;; word -- both spellings decoded by the single `kotoba-false?`.
;;
;; So this is not "admit the type and hope": the oracle executes one, which is
;; the precondition ADR 0219 set and the only reason it declined.
;;
;; `kotoba.verifier` re-derives this same boundary set independently and still
;; excludes `:bool`. Leaving it stricter is sound -- it can only reject -- but
;; it is NOT a no-op: a `:bool`-parameter module now passes target selection
;; and is refused later by `verify-native-artifact!` as "runtime KIR function
;; shape rejected". Widening `kotoba.verifier/native-boundary-type?` the same
;; way is the required follow-on before the native path works end to end.
(defn- native-boundary-type? [type schemas]
  (let [type (if (and (vector? type) (= 2 (count type)) (= :ref (first type))
                      (keyword? (second type)))
               (get schemas (second type) type)
               type)]
    (or (contains? #{:i64 :string :keyword} type)
        ;; `:bool` reaches admission through here: `native-word-value-type?`
        ;; has always listed it, and the `(not= :bool type)` guard that used to
        ;; wrap this whole `or` is what withheld it.
        (native-word-value-type? type)
        (native-scalar-record-type? type)
        (native-boundary-scalar-variant-type? type))))

(defn- native-private-handle-type? [type]
  ;; Native vectors and string indexes are context-owned handles. They are one
  ;; machine word at an internal call boundary, but they are not a host ABI:
  ;; an external caller cannot construct, validate, inspect, or release one
  ;; through a kexe export.
  ;; Keep this deliberately non-recursive so option/result ownership is not
  ;; widened as a side effect.
  (contains? #{:vector-i64 :vector-f64 :string-index} type))

(defn- native-function-boundary-type? [type schemas exported?]
  (or (native-boundary-type? type schemas)
      (and (not exported?) (native-private-handle-type? type))))

(defn- native-export-copy-result-type? [type]
  ;; KEXE export copy ABI v1 initially owns only top-level vector results.
  ;; Parameters still cannot be handles, and string-index has no wire form.
  (contains? #{:vector-i64 :vector-f64} type))

(defn only-native-word-typed-features? [hir]
  (letfn [(walk [form]
            (cond
              (or (string? form) (integer? form) (symbol? form)) true
              ;; A literal `true`/`false` is the ONLY way to produce a
              ;; genuine `:bool`-typed VALUE anywhere in this frontend's
              ;; type system today (confirmed by reading
              ;; `frontend.cljc/infer-expression-type`: every comparison,
              ;; including `=`, always returns `:i64`, never `:bool` -- see
              ;; `emit-record-get-of-new`'s own doc comment in both native
              ;; backends). Admitting it is the minimum needed to construct
              ;; a `:bool` record field at all; it is a plain 0/1 scalar
              ;; with no side effects, so admitting it in any expression
              ;; position (not only inside `record-new`) costs nothing extra
              ;; to verify and needs no narrower gating. `:keyword` remains
              ;; rejected -- this increment implements no native keyword
              ;; representation.
              (boolean? form) true
              ;; A keyword literal is now a value this backend can carry -- see
              ;; `native-word-field-types`. It was rejected here while no native
              ;; keyword representation existed.
              (keyword? form) true
              (seq? form)
              (let [[op & args] form]
                (cond
                  ;; Construction: every field value is walked; the type
                  ;; descriptor itself (args' first element) is compile-time
                  ;; sealed data, never walked as an expression.
                  (= op 'record-new)
                  (let [[type & values] args]
                    (and (native-scalar-record-type? type)
                         (= (count values) (count (nth type 2)))
                         (every? walk values)))
                  ;; Projection: the codegen backends require `value` to be
                  ;; a directly-nested, same-schema `record-new` -- that
                  ;; EXACT narrower shape is enforced by
                  ;; `emit-record-get-of-new` and (independently)
                  ;; `kotoba.verifier`'s own `record-get` case, not
                  ;; here, so this admission layer only needs to confirm
                  ;; the schema/field are well-formed and keep walking.
                  (= op 'record-get)
                  (let [[type value field] args]
                    (and (= 3 (count args))
                         (native-scalar-record-type? type)
                         (keyword? field)
                         (some #(= field (first %)) (nth type 2))
                         (walk value)))
                  ;; Construction: the tag must be a declared case (frontend
                  ;; already enforces this too, unconditionally, via
                  ;; `infer-expression-type`'s own "variant constructor tag
                  ;; is not declared" check -- re-derived here regardless,
                  ;; matching every other op-family's own independent-check
                  ;; discipline in this file); the payload is walked.
                  (= op 'variant-new)
                  (let [[type tag payload] args]
                    (and (= 3 (count args))
                         (native-scalar-variant-type? type)
                         (keyword? tag)
                         (some #(= tag (first %)) (nth type 2))
                         (walk payload)))
                  ;; Dispatch: mirrors `record-get`'s own admission shape --
                  ;; the codegen backends independently require `value` to
                  ;; be a directly-nested, same-schema `variant-new`
                  ;; (`emit-variant-match-of-new`, cross-checked again by
                  ;; `kotoba.verifier`'s own `variant-match` case),
                  ;; not enforced here; this layer only confirms every
                  ;; declared case has exactly one, exhaustively-ordered
                  ;; branch and keeps walking both the value and every
                  ;; branch body.
                  (= op 'variant-match)
                  (let [[type value branches] args]
                    (and (= 3 (count args))
                         (native-scalar-variant-type? type)
                         (vector? branches)
                         (= (mapv first (nth type 2)) (mapv first branches))
                         (every? #(and (vector? %) (= 3 (count %)) (symbol? (second %))) branches)
                         (walk value)
                         (every? (fn [[_ _binder body]] (walk body)) branches)))
                  ;; Native capability callbacks preserve sealed descriptors
                  ;; in KIR and pass either a scalar word or a validated
                  ;; pair-backed string/tagged-value handle at the
                  ;; already-type-checked machine-code boundary.
                  (= op 'typed-cap-call)
                  (let [[cap-id request-type result-type request] args]
                    (and (= 4 (count args))
                         #?(:clj (integer? cap-id)
                            :cljs (or (i64/bigint-value? cap-id) (integer? cap-id)))
                         (<= 0 cap-id 255)
                         (or (contains? #{[:i64 :i64] [:string :string]
                                          [:option-i64 :option-i64]
                                          [:result-i64 :result-i64]}
                                        [request-type result-type])
                             (native-provider-contract? cap-id request-type result-type))
                         (walk request)))
                  ;; f64 scalar arithmetic on native (ADR-2608030300). Same
                  ;; reasoning as `i32-operations` above: no new value
                  ;; representation, only i64 words -- here carrying an
                  ;; IEEE-754 bit pattern. Both native ISAs emit these
                  ;; directly.
                  (contains? '#{f64-add f64-sub f64-mul f64-div f64-min f64-max
                                f64-abs f64-neg f64-sqrt f64-from-bits f64-to-bits}
                             op)
                  (every? walk args)
                  ;; f32: binary32 arithmetic on native
                  ;; (ADR-kotoba-floating-point-on-native). Same admission
                  ;; shape as the f64 line above and for the same reason: no
                  ;; new value representation, only i64 words. The word an f32
                  ;; occupies is its binary32 pattern SIGN-EXTENDED from bit 31,
                  ;; which is what makes `f32-to-bits` an identity exactly the
                  ;; way `f64-to-bits` is -- and what makes `f32-from-bits` the
                  ;; one member of this family that is NOT an identity on
                  ;; native: it sign-extends, canonicalising a zero-extended
                  ;; u32 (what `kernel-load-u32` returns) into the same word a
                  ;; signed i32 already is.
                  ;;
                  ;; `f32-min`/`f32-max` are deliberately NOT here, and this is
                  ;; the one place the f32 family is narrower than the f64 one.
                  ;; x86's MINSS/MAXSS return the SECOND operand when either
                  ;; input is NaN; AArch64's FMIN/FMAX return the NaN; and this
                  ;; interpreter -- the definition -- uses Math/min, which also
                  ;; returns the NaN. So the f64 line above admits two
                  ;; operations on which x86 already disagrees with both the
                  ;; other ISA and the oracle. That is a pre-existing defect
                  ;; (recorded, not repaired here, because repairing it moves
                  ;; f64 goldens); this slice declines to duplicate it into a
                  ;; second width.
                  (contains? '#{f32-add f32-sub f32-mul f32-div
                                f32-abs f32-neg f32-sqrt f32-from-bits f32-to-bits}
                             op)
                  (every? walk args)
                  ;; Width conversions. Only the ones on which both ISAs and
                  ;; this interpreter agree for EVERY input:
                  ;;
                  ;;   f32-to-f64-exact   widening, no rounding, no domain
                  ;;   f64-to-f32-rounded round-to-nearest-even, overflow -> Inf
                  ;;   i64-to-f{32,64}-rounded  every i64 converts; RNE
                  ;;
                  ;; The `-checked` conversions (`i64-to-f32-checked`,
                  ;; `f32-to-i64-checked`, and their f64 twins) stay out: they
                  ;; TRAP here on inexactness and neither backend emits that
                  ;; check, so admitting them would let a program that must
                  ;; trap compute an answer instead.
                  ;;
                  ;; The truncating float->int conversions
                  ;; (`f32-to-i64-truncating`, `f64-to-i64-truncating`) stay out
                  ;; for a sharper reason: on an out-of-domain input there are
                  ;; three different answers. x86 CVTTSS2SI yields the integer
                  ;; indefinite value (INT64_MIN), AArch64 FCVTZS saturates, and
                  ;; this interpreter traps. Making them agree needs an emitted
                  ;; domain check, which is a separate increment.
                  (contains? '#{f32-to-f64-exact f64-to-f32-rounded
                                i64-to-f32-rounded i64-to-f64-rounded}
                             op)
                  (and (= 1 (count args)) (walk (first args)))
                  ;; f64 comparisons. These DO produce a genuine `:bool`-typed
                  ;; value, which the `true`/`false` comment above says only a
                  ;; literal could -- that was written before f64 reached
                  ;; native. It needs no new representation: the backends emit
                  ;; the same compare-and-set-flag pair the integer
                  ;; comparisons already do, into the same 0/1 word.
                  (contains? '#{f64-eq f64-lt f64-le f64-gt f64-ge
                                f64-unordered} op)
                  (and (= 2 (count args)) (every? walk args))
                  ;; f32 comparisons. Identical shape to the f64 line above:
                  ;; a compare-and-setcc pair into the same 0/1 word, over the
                  ;; single-precision compare (UCOMISS / FCMP s) instead of the
                  ;; double one. Unordered handling is the f64 path's, unchanged.
                  (contains? '#{f32-eq f32-lt f32-le f32-gt f32-ge
                                f32-unordered} op)
                  (and (= 2 (count args)) (every? walk args))
                  ;; Keyword OPERATIONS, which `native-word-field-types`'s
                  ;; comment listed as needing a general substring and
                  ;; concatenation over a runtime handle. Both now exist, and
                  ;; both backends desugar into exactly them -- no new value
                  ;; representation, since a keyword already travels as the
                  ;; one-word pair handle its printed text occupies.
                  (contains? '#{keyword-name keyword-from-string} op)
                  (and (= 1 (count args)) (walk (first args)))
                  ;; `bool-not` sits beside the option/result cases below for
                  ;; the same reason they do: it stays in
                  ;; `non-string-typed-ops` (the cljs gate shares that set) but
                  ;; the native backends now emit it, as a test-against-zero
                  ;; and setcc -- the very sequence every comparison there
                  ;; already produces. It needs no representation the one-word
                  ;; slice does not already have.
                  (= op 'bool-not)
                  (and (= 1 (count args)) (walk (first args)))
                  (= op 'option-some)
                  (and (= 1 (count args)) (walk (first args)))
                  (= op 'option-none)
                  (empty? args)
                  (= op 'option-some?)
                  (and (= 1 (count args)) (walk (first args)))
                  (= op 'option-value)
                  (and (= 2 (count args)) (every? walk args))
                  (contains? '#{result-ok result-err result-ok?} op)
                  (and (= 1 (count args)) (walk (first args)))
                  (contains? '#{result-value result-error} op)
                  (and (= 2 (count args)) (every? walk args))
                  (contains? '#{option-some-of option-some?-of} op)
                  (let [[type value] args]
                    (and (= 2 (count args))
                         (vector? type) (= :option (first type))
                         (native-word-value-type? type)
                         (walk value)))
                  (= op 'option-none-of)
                  (let [[type] args]
                    (and (= 1 (count args))
                         (vector? type) (= :option (first type))
                         (native-word-value-type? type)))
                  (= op 'option-value-of)
                  (let [[type value fallback] args]
                    (and (= 3 (count args))
                         (vector? type) (= :option (first type))
                         (native-word-value-type? type)
                         (walk value) (walk fallback)))
                  (= op 'option-match)
                  (let [[type value none-body binder some-body] args]
                    (and (= 5 (count args))
                         (vector? type) (= :option (first type))
                         (native-word-value-type? type)
                         (symbol? binder)
                         (walk value) (walk none-body) (walk some-body)))
                  (contains? '#{result-ok-of result-err-of result-ok?-of} op)
                  (let [[type value] args]
                    (and (= 2 (count args))
                         (vector? type) (= :result (first type))
                         (native-word-value-type? type)
                         (walk value)))
                  (contains? '#{result-value-of result-error-of} op)
                  (let [[type value fallback] args]
                    (and (= 3 (count args))
                         (vector? type) (= :result (first type))
                         (native-word-value-type? type)
                         (walk value) (walk fallback)))
                  (= op 'result-match-of)
                  (let [[type value ok-binder ok-body err-binder err-body] args]
                    (and (= 6 (count args))
                         (vector? type) (= :result (first type))
                         (native-word-value-type? type)
                         (symbol? ok-binder) (symbol? err-binder)
                         (walk value) (walk ok-body) (walk err-body)))
                  ;; `i32-operations` are admitted on native. They stay in
                  ;; `non-string-typed-ops` -- the CLJS gate below shares that
                  ;; set and does NOT implement them (measured 2026-08-03:
                  ;; 1/9 compile for cljs-kotoba-v1, against 9/9 for wasm32
                  ;; and js) -- so the exception is made here, for this target
                  ;; only, rather than by removing them from the shared set.
                  ;;
                  ;; Admitting them costs no new value representation: there
                  ;; is no `:i32` type (`kotoba.kir` mentions `:i32` exactly
                  ;; once, as a trap keyword), only i64 words carrying 32-bit
                  ;; wrapping, which both native backends now emit directly.
                  (contains? '#{i32-wrap u32-wrap i32-wrapping-add i32-wrapping-mul
                                i32-xor i32-shift-left i32-shift-right u32-shift-right}
                             op)
                  (every? walk args)

                  ;; `vector-i64` / `vector-f64` (ADR-2608030300). Admitted for
                  ;; the same reason `i32-operations` above are: they stay in
                  ;; `non-string-typed-ops` because the CLJS gate shares that
                  ;; set and does not implement them, so the exception is made
                  ;; here for this target rather than by widening the shared
                  ;; set.
                  ;;
                  ;; It costs no new value representation on native. A vector
                  ;; value is a one-word HANDLE into a host table -- the same
                  ;; width and the same discipline as the pair handle every
                  ;; option, result, string and keyword above already travels
                  ;; as. The elements live host-side, so nothing here needs a
                  ;; word wider than the slice machinery already has.
                  ;;
                  ;; The f64 family is admitted by the same clause and not a
                  ;; separate one, because a native f64 is already an i64 word
                  ;; carrying an IEEE-754 bit pattern (see the f64 arithmetic
                  ;; case above): an f64 vector is a vector of those words, so
                  ;; there is exactly one thing to admit.
                  ;;
                  ;; `vector-new` is variadic -- its arity is the literal's
                  ;; element count -- so unlike every fixed-arity case here it
                  ;; constrains only its elements. The backends expand it into
                  ;; one host call per element; the element-count bound that
                  ;; makes that expansion finite is `vector-item-limit`, and it
                  ;; is enforced where the value is built, not here.
                  (contains? '#{vector-new vector-alloc vector-count vector-get vector-at
                                vector-drop vector-assoc vector-assoc! vector-conj
                                vector-f64-new vector-f64-count vector-f64-get
                                vector-f64-at vector-f64-drop vector-f64-assoc
                                vector-f64-conj}
                             op)
                  (every? walk args)
                  ;; typed-set is a host-table handle, the same width as
                  ;; vector-i64. The type descriptor is sealed data, like
                  ;; record-new's first argument, and is not walked.
                  (= op 'typed-set-new)
                  (let [[type & items] args]
                    (and (vector? type) (= :set (first type))
                         (native-word-value-type? type)
                         (every? walk items)))
                  (= op 'typed-set-conj)
                  (let [[type value item] args]
                    (and (= 3 (count args))
                         (vector? type) (= :set (first type))
                         (native-word-value-type? type)
                         (walk value) (walk item)))
                  (contains? '#{typed-set-count typed-set-nth} op)
                  (let [[type & rest] args]
                    (and (vector? type) (= :set (first type))
                         (native-word-value-type? type)
                         (every? walk rest)))

                  ;; `string-contains?` / `string-replace-all` are admitted for
                  ;; exactly the reason `i32-operations` and the `vector-*`
                  ;; families above are, and by the same mechanism: they STAY in
                  ;; `non-string-typed-ops`, because the CLJS gate
                  ;; (`only-cljs-provider-typed-features?`, below) shares that
                  ;; set and does not lower them -- so the exception is made
                  ;; here, for this target only, rather than by removing the two
                  ;; entries from the shared set and silently widening the other
                  ;; gate with them.
                  ;;
                  ;; They cost no new value representation, no new context
                  ;; callback and no ABI change. `kotoba.native.string-search`
                  ;; (kotoba-native, ADR 0002, both ISAs from one shared source)
                  ;; lowers both by REWRITING them into the four string
                  ;; callbacks this slice has always had -- `string=?` (112),
                  ;; `string-concat` (120), `string-substring` (136),
                  ;; `string-code-point-at` (144) -- plus i64 arithmetic. That is
                  ;; the same desugaring discipline `keyword-name` /
                  ;; `keyword-from-string` above already follow, and the reason
                  ;; those two are admitted here is the reason these two are.
                  ;;
                  ;; The arities are pinned, not left to `every? walk`, matching
                  ;; the fixed-arity cases above and the exact shapes both
                  ;; backends dispatch on (`x86_64.cljc` 1189/1192,
                  ;; `aarch64.cljc` 988/991): any other arity has no lowering and
                  ;; must keep failing here rather than reaching a backend.
                  ;;
                  ;; Measured 2026-08-05 against kotoba-native
                  ;; `5df4d85`: this clause ALONE unlocks nothing -- the same
                  ;; two operations are refused a second time by
                  ;; `kotoba.verifier/string-operations`, which re-derives its
                  ;; own table. Both gates had to open; see this repo's ADR 0222.
                  (and (= op 'string-contains?) (= 2 (count args)))
                  (every? walk args)

                  (and (= op 'string-replace-all) (= 3 (count args)))
                  (every? walk args)

                  ;; Native `:document` is a string-shaped pair handle over
                  ;; UTF-8 EDN bytes. These two ops stay in
                  ;; `non-string-typed-ops` (the CLJS gate shares that set)
                  ;; and are admitted here only: they cost no new ABI word.
                  (and (= op 'document-edn-read) (= 1 (count args)))
                  (walk (first args))

                  (and (= op 'document-edn-print) (= 1 (count args)))
                  (walk (first args))

                  ;; A native string-index is lowered by kotoba-native into a
                  ;; private alternating key-handle/value vector. The five
                  ;; observable operations stay here in Kotoba machine code;
                  ;; no graph callback or context ABI slot is introduced.
                  (= op 'string-index-new)
                  (empty? args)

                  (= op 'string-index-count)
                  (and (= 1 (count args)) (walk (first args)))

                  (contains? '#{string-index-contains string-index-get} op)
                  (and (= 2 (count args)) (every? walk args))

                  (= op 'string-index-assoc)
                  (and (= 3 (count args)) (every? walk args))

                  ;; `let` MUST be walked explicitly. Without this case it fell
                  ;; to `:else`, which walks `args` -- and the first arg is the
                  ;; binding VECTOR, which is not `seq?` (vectors never are),
                  ;; so it reached the terminal `:else true` and its binding
                  ;; values were never inspected at all. The result was that
                  ;; the identical operation was gated when written directly
                  ;; and admitted when bound by a `let`:
                  ;;
                  ;;   (u32-wrap 1)            -> rejected, :phase :target
                  ;;   (let [a (u32-wrap 1)] a) -> admitted, reached the backend
                  ;;
                  ;; Only the f32/f64 families escaped, because
                  ;; `uses-f32?`/`uses-f64?` scan the whole tree separately.
                  ;; Every other typed family -- i32, typed maps, keywords,
                  ;; documents -- could hide behind a binding.
                  ;;
                  ;; This is also why `xorshift32` reached the backend while
                  ;; the `u32-wrap` it desugars into was gated: the frontend
                  ;; expands it to exactly such a `let`.
                  ;;
                  ;; Binders and the shape of the vector are checked but not
                  ;; walked: a binder is a symbol, and walking the vector as a
                  ;; whole is what the record/variant cases deliberately avoid
                  ;; for sealed type descriptors.
                  (= op 'let)
                  (let [[bindings body] args]
                    (and (vector? bindings)
                         (even? (count bindings))
                         (every? symbol? (take-nth 2 bindings))
                         (every? walk (take-nth 2 (rest bindings)))
                         (walk body)))

                  :else
                  (and (not (contains? non-string-typed-ops op))
                       (every? walk args))))
              :else true))]
    (let [schemas (:schemas hir)
          exports (set (:exports hir))]
      (every? (fn [{:keys [params param-types result body name]}]
                (let [exported? (contains? exports name)]
                  (and (every? #(native-function-boundary-type? % schemas exported?)
                                param-types)
                   ;; An INTERNAL function may also RETURN a record. It crosses
                   ;; the boundary boxed as a pair chain -- one word, built from
                   ;; the arena primitives this backend has always had -- which
                   ;; is the one shape flattening cannot reach, since a record is
                   ;; N slots and a function returns one word.
                   ;;
                   ;; The ENTRY is excluded: its result leaves the target for the
                   ;; host, which has no way to read a pair handle. That is the
                   ;; same split `kotoba.verifier` draws between its
                   ;; entry-result-types and function-result-types, and a
                   ;; deliberate negative in the compiler's own frontend
                   ;; extensions test pins it. The same reasoning excludes an
                   ;; option/result there: those are pair handles too.
                   (or (contains? #{:i64 :string :bool} result)
                       (and exported?
                            (not= name (:entry hir))
                            (empty? params)
                            (native-export-copy-result-type? result))
                       (and (not= name (:entry hir))
                            (native-function-boundary-type? result schemas exported?)))
                       (walk body))))
            (:functions hir)))))

(defn only-cljs-provider-typed-features?
  "True when typed values appear only as function boundary types and sealed
  typed-cap-call request/result descriptors. The CLJS backend owns the codec
  for those boundaries, but does not yet lower general typed construction or
  mutation operations."
  [hir]
  (letfn [(walk [form]
            (cond
              (or (string? form) (integer? form) (symbol? form)) true
              (or (keyword? form) (boolean? form)) false
              (seq? form)
              (let [[op & args] form]
                ;; Same hole `only-native-word-typed-features?` had, and here
                ;; the blanket `(vector? form) true` below made it explicit
                ;; without meaning to: a `let` binding vector is not a sealed
                ;; type descriptor, but it was admitted as one, so any typed
                ;; operation bound by a `let` skipped this gate entirely.
                (if (= op 'let)
                  (let [[bindings body] args]
                    (and (vector? bindings)
                         (even? (count bindings))
                         (every? symbol? (take-nth 2 bindings))
                         (every? walk (take-nth 2 (rest bindings)))
                         (walk body)))
                  (and (not (contains? non-string-typed-ops op))
                       (every? walk args))))
              ;; Type descriptors inside typed-cap-call are vectors and are
              ;; sealed constants, not runtime construction operations. This
              ;; stays a blanket admit because `let` -- the only other vector
              ;; that carries expressions -- is now destructured above and
              ;; never reaches here as a whole form.
              (vector? form) true
              :else false))]
    (every? #(walk (:body %)) (:functions hir))))

(defn uses-f64? [program]
  (boolean
   (some (fn [{:keys [param-types result body]}]
           (or (some #{:f64} param-types)
               (= :f64 result)
               (some #(and (seq? %)
                           (contains? '#{f64-to-bits f64-from-bits
                                         f64-add f64-sub f64-mul f64-div f64-min f64-max
                                         f64-neg f64-abs f64-sqrt
                                         f64-sin-quarter-turn f64-cos-quarter-turn
                                         f64-sin-bounded f64-cos-bounded
                                         f64-exp-near-zero f64-log-near-one f64-atan2-bounded
                                         f64-exp-bounded f64-log-bounded
                                         f64-eq f64-lt f64-le f64-gt f64-ge f64-unordered
                                         i64-to-f64-checked i64-to-f64-rounded
                                         f64-to-i64-checked f64-to-i64-truncating}
                                      (first %)))
                     (tree-seq coll? seq body))))
         (:functions program))))

(defn uses-f32? [program]
  (boolean
   (some (fn [{:keys [param-types result body]}]
           (or (some #{:f32} param-types)
               (= :f32 result)
               (some #(and (seq? %)
                           (contains? '#{f32-to-bits f32-from-bits
                                         f64-to-f32-rounded f32-to-f64-exact
                                         f32-add f32-sub f32-mul f32-div f32-min f32-max
                                         f32-neg f32-abs f32-sqrt
                                         f32-eq f32-lt f32-le f32-gt f32-ge f32-unordered
                                         i64-to-f32-checked i64-to-f32-rounded
                                         f32-to-i64-checked f32-to-i64-truncating}
                                      (first %)))
                     (tree-seq coll? seq body))))
         (:functions program))))

(defn host-stack-exhausted?
  "Is `e` the host running out of native stack, rather than a Kotoba trap?

   PUBLIC because the answer is not specific to this namespace. \"A host
   resource error must never escape the language boundary\" is one invariant,
   and by 2026-08-24 it had two hand-written implementations here and a third
   place that needed one -- `kotoba.compiler.frontend/analyze`, which recurses
   over the tree the desugar builds and let a raw `RangeError` out at a
   64-arm `case`. kotoba-sema already depends on this library, so the
   predicate is shared rather than copied a third time.

   Both branches ANSWER that question. An earlier version had `:clj true`,
   reasoning that only a `StackOverflowError` can reach the JVM catch -- true,
   and still the wrong shape: the predicate stopped asking on one runtime, and
   the `(throw e)` at the call site became unreachable there. A guard whose
   negative branch cannot be taken cannot be shown to discriminate, which is
   the defect this whole guard exists to prevent, one level up.

   On ClojureScript the catch is `:default`, so it has to tell this one host
   resource error apart from every `ex-info` the interpreter throws on purpose,
   and let those through untouched.

   V8 and JavaScriptCore raise `RangeError: Maximum call stack size exceeded`;
   SpiderMonkey raises `InternalError: too much recursion`. Matching on the
   message as well as the type is deliberate: a `RangeError` that is NOT stack
   exhaustion is a real error and must keep propagating."
  [e]
  #?(:clj (instance? StackOverflowError e)
     :cljs (let [message (str (.-message e))]
             (or (and (instance? js/RangeError e)
                      (str/includes? message "call stack"))
                 (str/includes? message "too much recursion")))))

(defn- trap! [reason data]
  (throw (ex-info (name reason) (merge {:phase :ir :trap reason} data))))

(defn- charge!
  "Decrement fuel. Optional `context` map is merged into :fuel-exhausted traps
  (T3.3: :function + :call-stack tip for source mapping)."
  ([fuel] (charge! fuel nil))
  ([fuel context]
   ;; `fuel`/`remaining` are interpreter-internal bookkeeping (a plain
   ;; counter), never a `.kotoba` VALUE, so this stays plain-number on both
   ;; runtimes -- no bigint coercion needed here or anywhere else fuel is
   ;; touched.
   (let [remaining (vswap! fuel dec)]
     (when (neg? remaining)
       (trap! :fuel-exhausted
              (merge {:limit default-fuel}
                     (when (map? context) context)))))))

(defn- f64-divide [left right]
  #?(:clj (let [^double left left ^double right right] (/ left right))
     :cljs (/ left right)))

(defn- as-f32 [value]
  #?(:clj (.floatValue ^Number value) :cljs (js/Math.fround value)))

(defn- f32-divide [left right]
  #?(:clj (let [^float left left ^float right right] (/ left right))
     :cljs (js/Math.fround (/ left right))))

(def ^:private quarter-turn 0.7853981633974483)

(defn- checked-quarter-turn [value]
  (when-not (and #?(:clj (Double/isFinite ^double value) :cljs (js/Number.isFinite value))
                 (<= (#?(:clj Math/abs :cljs js/Math.abs) value) quarter-turn))
    (trap! :f64-quarter-turn-domain {}))
  value)

(defn- f64-sin-quarter-turn [value]
  (let [value (checked-quarter-turn value)]
    (if (zero? value)
      value
      (let [z (* value value)
            p (+ -7.647163731819816e-13 (* z 2.8114572543455206e-15))
            p (+ 1.6059043836821613e-10 (* z p))
            p (+ -2.505210838544172e-8 (* z p))
            p (+ 2.7557319223985893e-6 (* z p))
            p (+ -0.0001984126984126984 (* z p))
            p (+ 0.008333333333333333 (* z p))
            p (+ -0.16666666666666666 (* z p))]
        (+ value (* (* value z) p))))))

(defn- f64-cos-quarter-turn [value]
  (let [value (checked-quarter-turn value)
        z (* value value)
        p (+ -1.1470745597729725e-11 (* z 4.779477332387385e-14))
        p (+ 2.08767569878681e-9 (* z p))
        p (+ -2.755731922398589e-7 (* z p))
        p (+ 0.0000248015873015873 (* z p))
        p (+ -0.001388888888888889 (* z p))
        p (+ 0.041666666666666664 (* z p))
        p (+ -0.5 (* z p))]
    (+ 1.0 (* z p))))

(def ^:private bounded-angle-limit 25735.927018207585)

(defn- reduce-bounded-angle [value]
  (when-not (and #?(:clj (Double/isFinite ^double value) :cljs (js/Number.isFinite value))
                 (<= (#?(:clj Math/abs :cljs js/Math.abs) value) bounded-angle-limit))
    (trap! :f64-bounded-angle-domain {}))
  (let [scaled (* value 0.6366197723675814)
        nearest (#?(:clj (fn [x] (if (neg? x) (Math/ceil (- x 0.5)) (Math/floor (+ x 0.5))))
                    :cljs (fn [x] (if (neg? x) (js/Math.ceil (- x 0.5)) (js/Math.floor (+ x 0.5)))))
                 scaled)
        reduced (- (- value (* nearest 1.5707963267948966))
                   (* nearest 6.123233995736766e-17))
        quadrant (mod #?(:clj (long nearest) :cljs nearest) 4)]
    [reduced quadrant]))

(defn- f64-sin-bounded [value]
  (let [[reduced quadrant] (reduce-bounded-angle value)]
    (case quadrant
      0 (f64-sin-quarter-turn reduced)
      1 (f64-cos-quarter-turn reduced)
      2 (- (f64-sin-quarter-turn reduced))
      (- (f64-cos-quarter-turn reduced)))))

(defn- f64-cos-bounded [value]
  (let [[reduced quadrant] (reduce-bounded-angle value)]
    (case quadrant
      0 (f64-cos-quarter-turn reduced)
      1 (- (f64-sin-quarter-turn reduced))
      2 (- (f64-cos-quarter-turn reduced))
      (f64-sin-quarter-turn reduced))))

(defn- f64-exp-near-zero [value]
  (when-not (and #?(:clj (Double/isFinite ^double value) :cljs (js/Number.isFinite value))
                 (<= (#?(:clj Math/abs :cljs js/Math.abs) value) 0.5))
    (trap! :f64-exp-near-zero-domain {}))
  (let [p (+ 2.8114572543455206e-15 (* value 1.5619206968586225e-16))
        p (+ 4.779477332387385e-14 (* value p))
        p (+ 7.647163731819816e-13 (* value p))
        p (+ 1.1470745597729725e-11 (* value p))
        p (+ 1.6059043836821613e-10 (* value p))
        p (+ 2.08767569878681e-9 (* value p))
        p (+ 2.505210838544172e-8 (* value p))
        p (+ 2.755731922398589e-7 (* value p))
        p (+ 2.7557319223985893e-6 (* value p))
        p (+ 0.0000248015873015873 (* value p))
        p (+ 0.0001984126984126984 (* value p))
        p (+ 0.001388888888888889 (* value p))
        p (+ 0.008333333333333333 (* value p))
        p (+ 0.041666666666666664 (* value p))
        p (+ 0.16666666666666666 (* value p))
        p (+ 0.5 (* value p))
        p (+ 1.0 (* value p))]
    (+ 1.0 (* value p))))

(defn- f64-log-near-one [value]
  (when-not (and #?(:clj (Double/isFinite ^double value) :cljs (js/Number.isFinite value))
                 (<= 0.75 value 1.5))
    (trap! :f64-log-near-one-domain {}))
  (let [y (/ (- value 1.0) (+ value 1.0))
        z (* y y)
        p (+ 0.05263157894736842 (* z 0.047619047619047616))
        p (+ 0.058823529411764705 (* z p))
        p (+ 0.06666666666666667 (* z p))
        p (+ 0.07692307692307693 (* z p))
        p (+ 0.09090909090909091 (* z p))
        p (+ 0.1111111111111111 (* z p))
        p (+ 0.14285714285714285 (* z p))
        p (+ 0.2 (* z p))
        p (+ 0.3333333333333333 (* z p))
        p (+ 1.0 (* z p))]
    (* (* 2.0 y) p)))

(defn- f64-atan-unit [value]
  (let [near-zero? (<= value 0.4142135623730951)
        t (if near-zero? value (/ (- value 1.0) (+ value 1.0)))
        z (* t t)
        p (+ 0.02702702702702703 (* z -0.02564102564102564))
        p (+ -0.02857142857142857 (* z p))
        p (+ 0.030303030303030304 (* z p))
        p (+ -0.03225806451612903 (* z p))
        p (+ 0.034482758620689655 (* z p))
        p (+ -0.037037037037037035 (* z p))
        p (+ 0.04 (* z p))
        p (+ -0.043478260869565216 (* z p))
        p (+ 0.047619047619047616 (* z p))
        p (+ -0.05263157894736842 (* z p))
        p (+ 0.058823529411764705 (* z p))
        p (+ -0.06666666666666667 (* z p))
        p (+ 0.07692307692307693 (* z p))
        p (+ -0.09090909090909091 (* z p))
        p (+ 0.1111111111111111 (* z p))
        p (+ -0.14285714285714285 (* z p))
        p (+ 0.2 (* z p))
        p (+ -0.3333333333333333 (* z p))
        p (+ 1.0 (* z p))
        angle (* t p)]
    (if near-zero? angle (+ 0.7853981633974483 angle))))

(defn- f64-atan2-bounded [y x]
  (when-not (and #?(:clj (Double/isFinite ^double y) :cljs (js/Number.isFinite y))
                 #?(:clj (Double/isFinite ^double x) :cljs (js/Number.isFinite x)))
    (trap! :f64-atan2-bounded-domain {}))
  (let [y-negative? (neg? (value/f64-to-i64-bits y))
        x-negative? (neg? (value/f64-to-i64-bits x))]
    (cond
      (zero? y) (if x-negative?
                  (if y-negative? -3.141592653589793 3.141592653589793)
                  y)
      (zero? x) (if y-negative? -1.5707963267948966 1.5707963267948966)
      :else (let [ay (#?(:clj Math/abs :cljs js/Math.abs) y)
                  ax (#?(:clj Math/abs :cljs js/Math.abs) x)
                  swap? (> ay ax)
                  ratio (if swap? (/ ax ay) (/ ay ax))
                  base (f64-atan-unit ratio)
                  angle (if swap? (- 1.5707963267948966 base) base)
                  angle (if x-negative? (- 3.141592653589793 angle) angle)]
              (if y-negative? (- angle) angle)))))

(def ^:private wide-exp-limit 354.891356446692)
(def ^:private wide-log-min 7.458340731200207e-155)
(def ^:private wide-log-max 1.3407807929942597e154)

(defn- binary-scale [exponent]
  (value/i64-bits-to-f64
   #?(:clj (* (+ (long exponent) 1023) 4503599627370496)
      :cljs (* (js/BigInt (+ exponent 1023)) (js/BigInt "4503599627370496")))))

(defn- f64-exp-bounded [value]
  (when-not (and #?(:clj (Double/isFinite ^double value) :cljs (js/Number.isFinite value))
                 (<= (#?(:clj Math/abs :cljs js/Math.abs) value) wide-exp-limit))
    (trap! :f64-exp-bounded-domain {}))
  (let [scaled (* value 1.4426950408889634)
        exponent (#?(:clj (fn [x] (long (if (neg? x) (Math/ceil (- x 0.5))
                                                    (Math/floor (+ x 0.5)))))
                     :cljs (fn [x] (if (neg? x) (js/Math.ceil (- x 0.5))
                                                   (js/Math.floor (+ x 0.5)))))
                  scaled)
        reduced (- (- value (* exponent 0.6931471805599453))
                   (* exponent 2.3190468138462996e-17))]
    (* (f64-exp-near-zero reduced) (binary-scale exponent))))

(defn- normalized-log-parts [input]
  #?(:clj
     (let [bits (value/f64-to-i64-bits input)
           exponent (- (quot bits 4503599627370496) 1023)
           mantissa (value/i64-bits-to-f64
                     (+ (bit-and bits 4503599627370495) 4607182418800017408))]
       (if (> mantissa 1.5) [(* mantissa 0.5) (inc exponent)] [mantissa exponent]))
     :cljs
     (let [bits (value/f64-to-i64-bits input)
           unit (js/BigInt "4503599627370496")
           field (/ bits unit)
           exponent (- (js/Number field) 1023)
           fraction (- bits (* field unit))
           mantissa (value/i64-bits-to-f64
                     (+ fraction (js/BigInt "4607182418800017408")))]
       (if (> mantissa 1.5) [(* mantissa 0.5) (inc exponent)] [mantissa exponent]))))

(defn- f64-log-bounded [input]
  (when-not (and #?(:clj (Double/isFinite ^double input) :cljs (js/Number.isFinite input))
                 (<= wide-log-min input wide-log-max))
    (trap! :f64-log-bounded-domain {}))
  (let [[mantissa exponent] (normalized-log-parts input)
        kernel (f64-log-near-one mantissa)]
    (+ (+ kernel (* exponent 0.6931471805599453))
       (* exponent 2.3190468138462996e-17))))

(defn- validate-runtime-value! [runtime-value type position]
  (if (and (vector? type) (= :ref (first type)) (= 2 (count type)))
    (if-let [descriptor (get *runtime-schemas* (second type))]
      (validate-runtime-value! runtime-value descriptor position)
      (trap! :unknown-schema-reference {:schema (second type) :position position}))
    (case type
    :i64
    (when-not #?(:clj (and (integer? runtime-value)
                            (<= Long/MIN_VALUE runtime-value Long/MAX_VALUE))
                 :cljs (and (i64/bigint-value? runtime-value)
                            (i64/in-i64-range? runtime-value)))
      (trap! :value-type-mismatch {:expected :i64 :position position}))

    :string
    (try
      (value/bounded-string! runtime-value value/string-value-byte-limit)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-string-value {:position position :message (ex-message error)})))

    :f64
    (when-not (value/f64-value? runtime-value)
      (trap! :value-type-mismatch {:expected :f64 :position position}))

    :f32
    (when-not (value/f32-value? runtime-value)
      (trap! :value-type-mismatch {:expected :f32 :position position}))

    :keyword
    (try
      (value/bounded-keyword! runtime-value value/keyword-value-byte-limit)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-keyword-value {:position position :message (ex-message error)})))

    :symbol
    (try
      (value/bounded-symbol! runtime-value value/symbol-value-byte-limit)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-symbol-value {:position position :message (ex-message error)})))

    :map
    (try
      (value/bounded-map! runtime-value)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-map-value {:position position :message (ex-message error)})))

    :bool
    ;; `:bool` is a plain 0/1 word, not a distinct runtime representation: the
    ;; wasm32 backend returns 1/0 and comparisons evaluate to 1/0 here, while
    ;; `true`/`false` literals stay booleans. Accepting both keeps the two
    ;; backends' results identical, which the dual-backend runner compares.
    (when-not (or (boolean? runtime-value)
                  #?(:clj (and (integer? runtime-value) (<= 0 runtime-value 1))
                     :cljs (and (i64/bigint-value? runtime-value)
                                (or (i64/k-zero? runtime-value)
                                    (= runtime-value i64/one)))))
      (trap! :value-type-mismatch {:expected :bool :position position}))

    :option-i64
    (try
      (value/bounded-option-i64! runtime-value)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-option-i64-value
               {:position position :message (ex-message error)})))

    :result-i64
    (try
      (value/bounded-result-i64! runtime-value)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-result-i64-value
               {:position position :message (ex-message error)})))

    :vector-i64
    (try
      (value/bounded-vector-i64! runtime-value)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-vector-i64-value
               {:position position :message (ex-message error)})))

    :vector-f64
    (try
      (value/bounded-vector-f64! runtime-value)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-vector-f64-value
               {:position position :message (ex-message error)})))

    :string-index
    (try
      (value/bounded-string-index! runtime-value)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-string-index-value
               {:position position :message (ex-message error)})))

    :disjoint-set-i64
    (try
      (value/bounded-disjoint-set-i64! runtime-value)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-disjoint-set-i64-value
               {:position position :message (ex-message error)})))

    :document
    (try
      (value/bounded-document! runtime-value)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-document-value
               {:position position :message (ex-message error)})))

    (try
      (value/bounded-typed-value! type runtime-value)
      (catch #?(:clj Exception :cljs :default) error
        (trap! :invalid-parametric-value
               {:type type :position position :message (ex-message error)})))))
  runtime-value)

;; #?(:cljs ...): every i64 arithmetic op coerces both operands via
;; `i64/->bigint` first (cheap no-op if already bigint) rather than relying
;; on callers to guarantee it -- JS throws outright ("Cannot mix BigInt and
;; other types") if a bigint and a plain number ever meet in `+`/`-`/`*`,
;; and at least one call site here (`-`'s unary-negation branch below,
;; `(i64-sub 0 (first xs))`) passes a literal plain-number `0` alongside a
;; bigint operand.
;; Each fn's whole BODY (not the `defn-` itself) is what branches --
;; wrapping several `defn-` forms together inside one `#?(:cljs (do ...))`
;; left later top-level code unable to resolve them under nbb's SCI
;; (confirmed live: "Unable to resolve symbol: i64-sub" even though it was
;; `defn-`'d moments earlier in the same file, inside such a `do`).
(defn- i64-add [x y]
  #?(:clj (unchecked-add (long x) (long y))
     :cljs (i64/wrap-i64 (+ (i64/->bigint x) (i64/->bigint y)))))
(defn- i64-sub [x y]
  #?(:clj (unchecked-subtract (long x) (long y))
     :cljs (i64/wrap-i64 (- (i64/->bigint x) (i64/->bigint y)))))
(defn- i64-mul [x y]
  #?(:clj (unchecked-multiply (long x) (long y))
     :cljs (i64/wrap-i64 (* (i64/->bigint x) (i64/->bigint y)))))

(defn- i32-wrap [value]
  #?(:clj (long (unchecked-int (long value)))
     :cljs (js/BigInt.asIntN 32 (i64/->bigint value))))

(defn- u32-wrap [value]
  #?(:clj (bit-and (long value) 0xffffffff)
     :cljs (js/BigInt.asUintN 32 (i64/->bigint value))))

(defn- checked-shift32 [value]
  (when-not #?(:clj (and (integer? value) (<= 0 value 31))
               :cljs (and (i64/bigint-value? value)
                          (<= i64/zero value (js/BigInt 31))))
    (trap! :i32-shift-count-out-of-range {:count value}))
  #?(:clj (int value) :cljs (js/Number value)))

;; ADR-2607254600 D1/D2. Wasm masks a shift count modulo the width; this
;; interpreter is the oracle the tests compare against, so it applies the same
;; [0,63] admission the frontend enforces and traps rather than wrapping.
(defn- checked-shift64 [value]
  (when-not #?(:clj (and (integer? value) (<= 0 value 63))
               :cljs (and (i64/bigint-value? value)
                          (<= i64/zero value (js/BigInt 63))))
    (trap! :i64-shift-count-out-of-range {:count value}))
  #?(:clj (int value) :cljs (js/Number value)))

(defn- i64-not [x]
  #?(:clj (bit-not (long x))
     :cljs (i64/wrap-i64 (bit-xor (i64/->bigint x) (js/BigInt -1)))))

(defn- i64-shl [value count]
  (let [shift (checked-shift64 count)]
    #?(:clj (bit-shift-left (long value) shift)
       ;; 2^shift is exact as a double for every shift in [0,63] (it is a power
       ;; of two), so the BigInt conversion is exact.
       :cljs (i64/wrap-i64 (* (i64/->bigint value)
                              (js/BigInt (js/Math.pow 2 shift)))))))

(defn- i64-shr [value count]
  (let [shift (checked-shift64 count)]
    #?(:clj (bit-shift-right (long value) shift)
       :cljs (i64/wrap-i64 (i64/ashr (i64/->bigint value) shift)))))

(defn- u64-shr [value count]
  (let [shift (checked-shift64 count)]
    #?(:clj (unsigned-bit-shift-right (long value) shift)
       ;; asUintN reinterprets the two's-complement bits as unsigned, so the
       ;; arithmetic shift below is a logical shift on that value.
       :cljs (i64/wrap-i64 (i64/ashr (js/BigInt.asUintN 64 (i64/->bigint value))
                                     shift)))))

;; ---------------------------------------------------------------------------
;; Kernel memory image.
;;
;; The kernel branch in `eval-expr` used to refuse every memory operation for
;; the same reason it still refuses `kernel-in-u8` and `kernel-read-msr`: this
;; interpreter cannot invent what the machine holds. That argument is right
;; about a device bus and a model-specific register. It is NOT right about
;; `kernel-load-u8` on a CALLER-OWNED buffer. Those bytes are not a property
;; of the machine; they are an argument, and a caller that supplies them has
;; told the oracle what they are.
;;
;; The lock pair was on the wrong side of that line for one release. Its
;; refusal was argued from a race -- who got there first -- but with an image
;; the operation is a compare-and-swap against a comparand and a replacement
;; the OPERATION fixes, over bytes the caller wrote, in an interpreter with one
;; thread. Determined, not invented. What it does not model is contention, and
;; the branch below says so where it is easy to read.
;;
;; Refusing them anyway cost every byte-walking decision object its off-target
;; oracle. aiueos records the consequence in its own words:
;; `contracts/dhcp-reply-valid-v1.edn` writes `:verification {:off-target
;; :impossible}`, and `scripts/aiueos/verify_value_runtime_cas_verify.clj`
;; records the worse one -- with no way to run the object it re-implemented
;; SHA-256 in Java and compared THAT against the contract's own expected
;; values, so its six vectors passed whatever the compiled object did.
;;
;; So an image is optional and absent by default. Without one the refusal is
;; byte for byte what it was. With one, the checks each backend emits are
;; reproduced exactly -- profile maximum, non-null base, index inside the
;; declared window -- and a violation traps as `:kernel-memory-fault`, which is
;; that backend's UD2 and therefore a real answer.
;;
;; An access that is LEGAL for the machine but reaches outside the supplied
;; image is neither an admission nor a refusal. It gets its own trap,
;; `:kernel-memory-outside-image`: the oracle could not answer, and that must
;; never be readable as either verdict.

(def ^:private kernel-memory-profile
  "op -> [profile-maximum access-width-bytes], transcribed from
  `kotoba.native.x86_64` and checked against `kotoba.native.aarch64`, which
  admit identical bounds on both ISAs. An op absent from this map is not a
  memory op."
  '{kernel-load-u8     [512 1]
    kernel-load-u8-4k  [4096 1]
    kernel-load-u8-16k [16384 1]
    kernel-store-u8    [512 1]
    kernel-store-u8-4k [4096 1]
    kernel-load-u32    [512 4]
    kernel-store-u32   [512 4]
    ;; The lock pair's ceiling is 4096, not 512: `kotoba.native.machine-ir`
    ;; gives them `[:gmir/kernel-try-lock-u32 4096]`. The width is the same
    ;; four bytes, so the `length - index >= 4` rule applies to them too.
    kernel-try-lock-u32 [4096 4]
    kernel-unlock-u32   [4096 4]})

(defn- word-above?
  "Unsigned `>` on two i64 runtime words -- the backends' `ja`."
  [a b]
  #?(:clj (pos? (Long/compareUnsigned (long a) (long b)))
     :cljs (> (js/BigInt.asUintN 64 (i64/->bigint a))
              (js/BigInt.asUintN 64 (i64/->bigint b)))))

(defn- word-at-least?
  "Unsigned `>=` -- the backends' `jae`."
  [a b]
  #?(:clj (not (neg? (Long/compareUnsigned (long a) (long b))))
     :cljs (>= (js/BigInt.asUintN 64 (i64/->bigint a))
               (js/BigInt.asUintN 64 (i64/->bigint b)))))

(defn- word-zero? [x]
  #?(:clj (zero? (long x)) :cljs (i64/k-zero? (i64/->bigint x))))

(defn- word-plus [a b]
  #?(:clj (unchecked-add (long a) (long b))
     :cljs (i64/wrap-i64 (+ (i64/->bigint a) (i64/->bigint b)))))

(defn- word-minus [a b]
  #?(:clj (unchecked-subtract (long a) (long b))
     :cljs (i64/wrap-i64 (- (i64/->bigint a) (i64/->bigint b)))))

(defn- word-byte
  "The byte `shift` bits up in `value`, as a plain number -- what the backends
  narrow to before `mov [mem],al`."
  [value shift]
  #?(:clj (int (bit-and (unsigned-bit-shift-right (long value) shift) 255))
     :cljs (js/Number (js/BigInt.asUintN
                       8 (/ (js/BigInt.asUintN 64 (i64/->bigint value))
                            (js/BigInt (bit-shift-left 1 shift)))))))

(defn- ->word [n]
  #?(:clj (long n) :cljs (i64/->bigint n)))

(defn- kernel-window-check!
  "The three checks `emit-kernel-load-u8` and its siblings emit, in their
  order, so a program that traps here traps on the machine and vice versa."
  [op base length index]
  (let [[maximum width] (get kernel-memory-profile op)]
    (when (word-above? length maximum)
      (trap! :kernel-memory-fault {:operation op :check :length-above-profile-maximum
                                   :length length :maximum maximum}))
    (when (word-zero? base)
      (trap! :kernel-memory-fault {:operation op :check :null-base}))
    (if (= width 1)
      (when (word-at-least? index length)
        (trap! :kernel-memory-fault {:operation op :check :index-outside-window
                                     :index index :length length}))
      ;; Two checks, in the order `kotoba.native.machine-ir` lowers them:
      ;; `index < length`, and then `length - index >= 4`. Neither can wrap.
      ;;
      ;; The first version of this read only kotoba-native's `emit-kernel-load-u32`,
      ;; which computes `index + 4` with `lea` and compares THAT -- a form where
      ;; an index in [2^64-4, 2^64-1] wraps to 0..3 and addresses the four bytes
      ;; before the window -- and reproduced it deliberately, on the reasoning
      ;; that an oracle must not refuse what the machine admits. The reasoning
      ;; was right and the reading was wrong: those two functions are a fallback
      ;; that `emit-program` does not route u32 through (measured 2026-08-31 by
      ;; instrumenting both vars, including a recursive walker that leaves the
      ;; pilot path), and the shipping lowering has always used the
      ;; non-wrapping form. kotoba-native now guards the fallback too, so both
      ;; paths agree with what is modelled here.
      (do
        (when (word-at-least? index length)
          (trap! :kernel-memory-fault {:operation op :check :index-outside-window
                                       :index index :length length}))
        (when (word-above? 4 (word-minus length index))
          (trap! :kernel-memory-fault {:operation op :check :four-byte-access-outside-window
                                       :index index :length length}))))))

(defn- image-slot
  "Index of `pointer + index` within the supplied image, or a refusal to
  answer. Never a machine verdict: see the header above."
  [memory op pointer index width]
  (let [image @(:bytes memory)
        limit (count image)
        slot (word-minus (word-plus pointer index) (:base memory))
        inside? #?(:clj (and (<= 0 slot) (<= (+ slot width) limit))
                   :cljs (and (>= slot (js/BigInt 0))
                              (<= (+ slot (js/BigInt width)) (js/BigInt limit))))]
    (when-not inside?
      (trap! :kernel-memory-outside-image
             {:operation op :pointer pointer :index index :width width
              :image-base (:base memory) :image-bytes limit}))
    #?(:clj slot :cljs (js/Number slot))))

(defn- kernel-memory-call!
  "Evaluate one kernel memory operation against a supplied image."
  [op memory values]
  (if (= op 'kernel-subregion)
    ;; `(kernel-subregion base length offset sublen)` -> base+offset, trapping
    ;; unless the sub-window fits. No memory is touched, so no image is read:
    ;; a derived pointer is arithmetic, and the load that uses it is what has
    ;; to be inside the image.
    (let [[base length offset sublen] values]
      (when (word-zero? base)
        (trap! :kernel-memory-fault {:operation op :check :null-base}))
      (when (word-above? offset length)
        (trap! :kernel-memory-fault {:operation op :check :offset-outside-window
                                     :offset offset :length length}))
      (let [remaining (word-minus length offset)]
        (when (word-above? sublen remaining)
          (trap! :kernel-memory-fault {:operation op :check :subwindow-outside-window
                                       :sublength sublen :remaining remaining})))
      (word-plus base offset))
    (let [[base length index value] values
          [_ width] (get kernel-memory-profile op)
          _ (kernel-window-check! op base length index)
          slot (image-slot memory op base index width)
          bytes (:bytes memory)]
      (case op
        (kernel-load-u8 kernel-load-u8-4k kernel-load-u8-16k)
        (->word (nth @bytes slot))

        kernel-load-u32
        (let [image @bytes]
          (->word (+ (nth image slot)
                     (* 256 (nth image (+ slot 1)))
                     (* 65536 (nth image (+ slot 2)))
                     (* 16777216 (nth image (+ slot 3))))))

        (kernel-store-u8 kernel-store-u8-4k)
        (do (vswap! bytes assoc slot (word-byte value 0))
            ;; RAX still holds `value` after `mov [rdx+rdi],al`.
            value)

        kernel-store-u32
        (do (vswap! bytes assoc
                    slot (word-byte value 0)
                    (+ slot 1) (word-byte value 8)
                    (+ slot 2) (word-byte value 16)
                    (+ slot 3) (word-byte value 24))
            value)

        ;; `lock cmpxchg` with the operation's own comparand and replacement:
        ;; 0 -> 1 to take, 1 -> 0 to release. Returns 1 when the swap happened.
        (kernel-try-lock-u32 kernel-unlock-u32)
        (let [image @bytes
              observed (+ (nth image slot)
                          (* 256 (nth image (+ slot 1)))
                          (* 65536 (nth image (+ slot 2)))
                          (* 16777216 (nth image (+ slot 3))))
              [expected desired] (if (= op 'kernel-try-lock-u32) [0 1] [1 0])]
          (if (= observed expected)
            (do (vswap! bytes assoc
                        slot (bit-and desired 255)
                        (+ slot 1) 0 (+ slot 2) 0 (+ slot 3) 0)
                (->word 1))
            (->word 0)))))))

(defn- validated-memory
  "Admit an optional `:memory` image for `execute`."
  [memory]
  (when memory
    (let [{:keys [base bytes]} memory]
      (when-not (and (some? base) (not (word-zero? base)))
        (throw (ex-info "memory image base must be a non-zero address"
                        {:phase :ir :base base})))
      ;; A plain vector would make every store invisible to the caller, and an
      ;; oracle whose writes cannot be read back is the failure this whole
      ;; change exists to remove. Refuse it rather than copy it.
      (when-not (volatile? bytes)
        (throw (ex-info "memory image bytes must be a volatile! holding a vector"
                        {:phase :ir})))
      (when-not (and (vector? @bytes)
                     (every? #(and (integer? %) (<= 0 % 255)) @bytes))
        (throw (ex-info "memory image bytes must be a vector of 0..255"
                        {:phase :ir :count (count @bytes)})))
      {:base (->word base) :bytes bytes})))

(defn- i32-add [x y]
  #?(:clj (long (unchecked-add-int (unchecked-int (long x)) (unchecked-int (long y))))
     :cljs (js/BigInt.asIntN 32 (+ (i32-wrap x) (i32-wrap y)))))

(defn- i32-mul [x y]
  #?(:clj (long (unchecked-multiply-int (unchecked-int (long x)) (unchecked-int (long y))))
     :cljs (js/BigInt.asIntN 32 (* (i32-wrap x) (i32-wrap y)))))

(defn- i32-xor [x y]
  #?(:clj (long (bit-xor (unchecked-int (long x)) (unchecked-int (long y))))
     :cljs (js/BigInt.asIntN 32 (bit-xor (i32-wrap x) (i32-wrap y)))))

(defn- i32-shl [value count]
  (let [shift (checked-shift32 count)]
    #?(:clj (i32-wrap (bit-shift-left (unchecked-int (long value)) shift))
       :cljs (js/BigInt.asIntN
              32 (* (i32-wrap value) (js/BigInt (js/Math.pow 2 shift)))))))

(defn- i32-shr [value count]
  (let [shift (checked-shift32 count)]
    #?(:clj (long (bit-shift-right (unchecked-int (long value)) shift))
       :cljs (i64/ashr (i32-wrap value) shift))))

(defn- u32-shr [value count]
  (let [shift (checked-shift32 count)]
    #?(:clj (u32-wrap (unsigned-bit-shift-right (u32-wrap value) shift))
       :cljs (/ (u32-wrap value) (js/BigInt (js/Math.pow 2 shift))))))

(defn- xorshift32 [value]
  (let [x (u32-wrap value)
        x (u32-wrap (bit-xor x #?(:clj (bit-shift-left x 13)
                                  :cljs (* x (js/BigInt 8192)))))
        x (u32-wrap (bit-xor x (u32-shr x #?(:clj 17 :cljs (js/BigInt 17)))))
        x (u32-wrap (bit-xor x #?(:clj (bit-shift-left x 5)
                                  :cljs (* x (js/BigInt 32)))))]
    x))

(declare eval-expr read-pair)

(defn- validated-closure-param-indexes
  "Validate the internal representation refinement used for parameters that
  carry compiler closure handles. Their ABI type remains i64; the metadata
  lets representation-aware runtimes check the pair shape without widening
  every ordinary i64 parameter."
  [function]
  (let [present? (contains? function :closure-param-indexes)
        indexes (:closure-param-indexes function)
        params (:params function)
        param-types (or (:param-types function)
                        (vec (repeat (count params) :i64)))]
    (when (and present?
               (not (and (vector? indexes)
                         (= indexes (vec (sort (distinct indexes))))
                         (every? #(and (integer? %) (<= 0 %)
                                       (< % (count params))
                                       (= :i64 (nth param-types % nil)))
                                 indexes))))
      (throw (ex-info "closure parameter indexes are malformed"
                      {:phase :ir :function (:name function)
                       :closure-param-indexes indexes})))
    (or indexes [])))

(defn- validated-i64-pair-chain-param-indexes
  "Validate parameters whose i64 ABI word denotes a zero-terminated, bounded
  pair-chain of i64 values. This is distinct from a closure pair even though
  both representations use the pair heap on KIR/Wasm."
  [function]
  (let [present? (contains? function :i64-pair-chain-param-indexes)
        indexes (:i64-pair-chain-param-indexes function)
        closure-indexes (set (validated-closure-param-indexes function))
        params (:params function)
        param-types (or (:param-types function)
                        (vec (repeat (count params) :i64)))]
    (when (and present?
               (not (and (vector? indexes)
                         (= indexes (vec (sort (distinct indexes))))
                         (not-any? closure-indexes indexes)
                         (every? #(and (integer? %) (<= 0 %)
                                       (< % (count params))
                                       (= :i64 (nth param-types % nil)))
                                 indexes))))
      (throw (ex-info "i64 pair-chain parameter indexes are malformed"
                      {:phase :ir :function (:name function)
                       :i64-pair-chain-param-indexes indexes})))
    (or indexes [])))

(defn- validated-closure-result? [function]
  (let [present? (contains? function :closure-result?)
        closure-result? (:closure-result? function)]
    (when (and present?
               (not (and (true? closure-result?)
                         (= :i64 (or (:result function) :i64)))))
      (throw (ex-info "closure result refinement is malformed"
                      {:phase :ir :function (:name function)
                       :closure-result? closure-result?})))
    (true? closure-result?)))

(defn- validate-closure-handle!
  "Check the interpreter's heap-backed closure pair and its bounded capture
  chain. Wasm/native retain the same i64 word ABI; this is the reference
  representation check corresponding to restricted ESM's physical pair guard."
  [heap handle position]
  (let [lambda-id (read-pair heap handle 0)
        captures (read-pair heap handle 1)]
    (validate-runtime-value! lambda-id :i64 (assoc position :closure-id true))
    (loop [chain captures
           count 0]
      (cond
        #?(:clj (zero? chain) :cljs (i64/k-zero? chain)) nil
        (>= count 5) (trap! :invalid-closure-capture-chain
                            {:position position :max-captures 5})
        :else (do (read-pair heap chain 0)
                  (recur (read-pair heap chain 1) (inc count)))))))

(defn- validate-i64-pair-chain!
  [heap handle position]
  (loop [chain handle
         count 0]
    (cond
      #?(:clj (zero? chain) :cljs (i64/k-zero? chain)) nil
      (>= count 4) (trap! :i64-pair-chain-limit
                          {:position position :max-items 4})
      :else (let [item (read-pair heap chain 0)]
              (validate-runtime-value! item :i64 (assoc position :pair-item count))
              (recur (read-pair heap chain 1) (inc count))))))

;; T7.4 / T7.1: self-tail calls on frontend-synthesized `__kotoba_loop_N`
;; helpers trampoline in the KIR interpreter so 10k+ iterations do not blow
;; the host JVM stack. T7.1: **zero-charge** on trampoline re-entry of the
;; same loop helper (first entry still charges 1 unit per T7.2). Hosts must
;; still wall-clock-bound adversarial infinite loops.
(def ^:private trampoline-tag ::trampoline)

(defn- loop-helper-name?
  "True for sequential loop-helper names emitted by the frontend desugar."
  [sym]
  (and (symbol? sym)
       (nil? (namespace sym))
       (boolean (re-matches #"__kotoba_loop_\d+" (name sym)))))

(defn- loop-helper-self-calls-off-tail
  "Calls to `fname` inside `body` that are NOT in tail position.

   The trampoline's admission is `(and (loop-helper-name? op) (= op tip))`,
   which says `self-call` and nothing about where the call sits. A self-call
   in an argument position therefore evaluates to a trampoline MARKER, and
   `invoke-function` only ever unwraps a marker that comes back as the whole
   body result -- so the marker becomes an operand. Measured 2026-08-24 on a
   hand-written module:

     (defn __kotoba_loop_1 [n] (if (<= n 0) 0 (+ 1 (__kotoba_loop_1 (- n 1)))))
     => Cannot convert {:kotoba.kir/trampoline true, :function __kotoba_loop_1,
                        :values [2]} to a BigInt

   Unreachable from `.kotoba` source: the frontend emits these names only for
   `loop`/`recur` desugar and always in tail position, and
   `reserved-binding-name?` keeps user code away from the `__kotoba_` prefix.
   But `kotoba.hir/v3` is an accepted input surface, so without this check
   `hir/validate!` admits a module the interpreter cannot run correctly, and
   the failure surfaces as an internal representation inside an error message
   rather than as a refusal in the language's own vocabulary.

   Rejecting is right rather than making it work: a non-tail self-call in a
   helper the frontend guarantees is tail-recursive did not come from the
   frontend, and the interpreter has no obligation to interpret it."
  [fname body]
  (let [found (volatile! [])]
    (letfn [(walk [form tail?]
              (when (and (seq? form) (seq form))
                (let [[op & args] form]
                  (cond
                    ;; `(let [n v n v] body)` -- one body form, and the
                    ;; binding VALUES are not in tail position.
                    (= op 'let)
                    (let [[bindings body-form] args]
                      (doseq [v (take-nth 2 (rest bindings))] (walk v false))
                      (walk body-form tail?))

                    (= op 'if)
                    (let [[test then else] args]
                      (walk test false)
                      (walk then tail?)
                      (walk else tail?))

                    (= op 'do)
                    (do (doseq [f (butlast args)] (walk f false))
                        (walk (last args) tail?))

                    :else
                    (do (when (and (= op fname) (not tail?))
                          (vswap! found conj form))
                        (doseq [a args] (walk a false)))))))]
      (walk body true))
    @found))

(defn- reject-loop-helper-self-calls-off-tail! [hir]
  (doseq [function (:functions hir)
          :when (loop-helper-name? (:name function))
          :let [offenders (loop-helper-self-calls-off-tail (:name function)
                                                           (:body function))]
          :when (seq offenders)]
    (throw (ex-info "loop-helper self-call outside tail position"
                    {:phase :ir
                     :rejected :loop-helper-self-call-not-in-tail-position
                     :function (:name function)
                     :calls (vec (take 4 offenders))
                     :count (count offenders)}))))

(defn- trampoline-call [fname values]
  {trampoline-tag true :function fname :values values})

(defn- trampoline-call? [x]
  (and (map? x) (true? (get x trampoline-tag))))

(defn- invoke-function [function values functions fuel heap call-stack cap-call]
  (when-not function
    (trap! :unknown-function {}))
  (loop [function function
         values values
         stack call-stack
         charge? true]
    (when-not (= (count (:params function)) (count values))
      (trap! :arity-mismatch {:function (:name function)
                              :expected (count (:params function))
                              :actual (count values)}))
    (let [param-types (or (:param-types function)
                          (vec (repeat (count (:params function)) :i64)))
          closure-param-indexes (set (validated-closure-param-indexes function))
          pair-chain-param-indexes
          (set (validated-i64-pair-chain-param-indexes function))
          fname (:name function)]
      (doseq [[index parameter runtime-value type]
              (map vector (range) (:params function) values param-types)]
        (validate-runtime-value! runtime-value type {:function fname
                                                     :parameter parameter})
        (when (contains? closure-param-indexes index)
          (validate-closure-handle! heap runtime-value
                                    {:function fname :parameter parameter}))
        (when (contains? pair-chain-param-indexes index)
          (validate-i64-pair-chain! heap runtime-value
                                    {:function fname :parameter parameter})))
      ;; Charge once on first entry; loop-helper self-tail re-entries are free (T7.1).
      (let [stack' (conj stack fname)]
        (when charge?
          (charge! fuel {:function fname
                         :call-stack (vec (take-last 8 stack'))
                         :hint "export or loop-helper name; approximate form not tracked"}))
        (let [result (eval-expr (:body function) (zipmap (:params function) values) functions
                                fuel heap stack' cap-call)]
          (if (trampoline-call? result)
            (let [next-name (:function result)
                  next-fn (get functions next-name)
                  self-loop? (and (loop-helper-name? next-name) (= next-name fname))]
              (when-not next-fn
                (trap! :unknown-function {:function next-name}))
              ;; Self-tail on the same helper keeps stack tip (no host growth)
              ;; and skips fuel charge (zero-charge recur for desugared loop).
              (recur next-fn (:values result)
                     (if self-loop? stack stack')
                     (not self-loop?)))
            (do
              (validate-runtime-value! result (or (:result function) :i64)
                                       {:function fname :result true})
              (when (validated-closure-result? function)
                (validate-closure-handle! heap result
                                          {:function fname :result true}))
              result)))))))

(defn- allocate-pair! [heap left right]
  (let [{:keys [cells capacity]} heap
        index (count @cells)]
    (when (>= index capacity)
      (trap! :heap-exhausted {:capacity capacity}))
    (vswap! cells conj [left right])
    ;; The returned handle re-enters the value stream as an ordinary
    ;; `.kotoba` i64 value (e.g. it may later be compared, or passed to
    ;; `pair-first`) -- coerce to bigint here, the one place a plain-number
    ;; heap index (`index`, interpreter-internal) becomes a kotoba value.
    #?(:clj (inc index) :cljs (i64/->bigint (inc index)))))

(defn- read-pair [heap handle slot]
  #?(:clj
     (when-not (and (integer? handle) (pos? handle)
                    (<= handle (count @(:cells heap))))
       (trap! :invalid-pair-handle {:handle handle}))
     :cljs
     (when-not (and (i64/bigint-value? handle) (i64/k-pos? handle)
                    (<= handle (count @(:cells heap))))
       (trap! :invalid-pair-handle {:handle handle})))
  ;; `handle` is a kotoba VALUE (bigint on :cljs); vector indexing needs a
  ;; plain number. Safe to narrow: heap capacity is bounded to
  ;; `default-pair-capacity`/`pair-capacity`, far inside the safe-integer
  ;; range, and an out-of-range handle already trapped above.
  (let [index #?(:clj (dec handle) :cljs (dec (js/Number handle)))]
    (nth (nth @(:cells heap) index) slot)))

;; kgraph-* (ADR-2607198300): an all-integer EAVT datom store, the native
;; (JVM/Node/browser-free, `:x86_64-kotoba-v1`/`:aarch64-kotoba-v1`) analog of
;; kotoba-lang/kotoba's string/EDN-based kgraph-assert!/kgraph-query. There is
;; no addressable buffer in this native backend to carry EDN text (see
;; frontend.cljc/backend -- values here are i64 only), so entity/attribute/
;; value are caller-assigned integer ids rather than strings. Shares the
;; `heap` map's existing threading (a new `:datoms` key alongside `:cells`)
;; instead of adding a new interpreter parameter.
(defn- assert-datom! [heap e a v]
  (let [{:keys [datoms kgraph-capacity]} heap]
    (when (>= (count @datoms) kgraph-capacity)
      (trap! :kgraph-exhausted {:capacity kgraph-capacity}))
    (vswap! datoms conj [e a v])
    #?(:clj 1 :cljs i64/one)))

(defn- datom-value [datoms e a not-found]
  ;; Last-write-wins, matching kgraph-lang/kotoba's own kgraph-query
  ;; semantics for a point (entity, attribute) lookup.
  (reduce (fn [result [de da dv]]
            (if (and (= de e) (= da a)) dv result))
          not-found datoms))

(defn- get-datom [heap e a]
  (datom-value @(:datoms heap) e a #?(:clj Long/MIN_VALUE :cljs i64/min-i64)))

(defn- distinct-entities [datoms a]
  (->> datoms
       (filter (fn [[_ da _]] (= da a)))
       (map first)
       distinct
       vec))

(defn- count-entities [heap a]
  (let [n (count (distinct-entities @(:datoms heap) a))]
    #?(:clj (long n) :cljs (i64/->bigint n))))

(defn- entity-at [heap a index]
  #?(:clj
     (when-not (and (integer? index) (<= 0 index))
       (trap! :invalid-kgraph-index {:index index}))
     :cljs
     (when-not (and (i64/bigint-value? index) (not (i64/k-neg? index)))
       (trap! :invalid-kgraph-index {:index index})))
  (let [entities (distinct-entities @(:datoms heap) a)
        i #?(:clj index :cljs (js/Number index))]
    (when-not (< i (count entities))
      (trap! :invalid-kgraph-index {:index index}))
    (nth entities i)))

(defn- compact-host-index [index size code]
  (when-not (and #?(:clj (integer? index) :cljs (i64/bigint-value? index))
                 (not (neg? index)) (< index size))
    (trap! code {:index index :size size}))
  #?(:clj (int index) :cljs (js/Number index)))

(defn- container-host-index
  "An already bounds-checked i64 index, as an index the host container
  accepts. On cljs an i64 is a BigInt and `nth` / `assoc` / `subvec` / `inc`
  all reject one, so every in-range document index threw while the
  out-of-range one returned a clean `none` -- the broken path was the quieter
  of the two. Callers keep their own i64 range check; this only converts the
  value they already admitted."
  [index]
  #?(:clj (int index) :cljs (js/Number index)))

(defn- disjoint-root [parents start]
  (loop [current start remaining (inc (count parents))]
    (when (zero? remaining)
      (trap! :invalid-disjoint-set-i64-value {:reason :parent-cycle}))
    (let [parent (compact-host-index (nth parents current) (count parents)
                                     :invalid-disjoint-set-i64-value)]
      (if (= parent current)
        current
        (recur parent (dec remaining))))))

;; The interpreter's single truth convention, in one place.
;;
;; `:bool` is a plain 0/1 word inside a module, but a host boolean may also
;; appear -- 38d1bd0 boxes one at the execute boundary and states that both
;; remain acceptable internally. So deciding "is this false" must decode both.
;;
;; `if` had this inline and was correct. `bool-not` re-derived it as a bare
;; `(not value)`, which misreads the word: 0 is truthy in Clojure, so
;; `(not 0)` and `(not 1)` are both false and `bool-not` returned FALSE FOR
;; EVERY INPUT. Comparisons evaluate to 0/1, so `(bool-not (= a b))` -- the
;; obvious way to write "not equal" -- was constantly false. Because this
;; interpreter is the oracle the other targets are checked against, that was
;; the language's definition, not merely a reference-only slip.
(defn- kotoba-false? [value]
  (if (boolean? value)
    (not value)
    #?(:clj (zero? value) :cljs (i64/k-zero? value))))

(defn eval-expr
  "Evaluate one KIR form.

   `if` and `let` do NOT recur into the host stack. Both end in a tail
   position -- the chosen branch, the `let` body -- so they rebind and loop
   here instead of calling this function again.

   That is not a micro-optimisation. Everything this compiler builds out of
   chained conditionals is a nested `if`: `cond` and `case` desugar to one,
   `do` desugars to nested `let`, and the closure `invoke` dispatcher is a
   linear chain over every candidate lambda in the module. With recursion, the
   depth those reach was bounded by the HOST stack, which is why the constant
   oracle's ceiling moved with the SIZE OF THE MODULE rather than with the
   program being folded. Measured 2026-08-24 on nbb 1.5.212: a `lazy-map` over
   an infinite sequence folded, and adding ONE unrelated function that also
   used `lazy-map` made the identical `main` exhaust the host stack."
  [form env functions fuel heap call-stack cap-call]
  (loop [form form env env]
   (cond
    #?(:clj (integer? form)
       ;; A literal here may be a bigint (read from `.kotoba` source) or a
       ;; plain number (synthesized by `kotoba.compiler.frontend`'s
       ;; desugaring, e.g. `when`'s trailing `0`) -- `kotoba-integer?`'s own
       ;; docstring there explains why both are admitted; this is the
       ;; single point that coerces either into the bigint value stream
       ;; every downstream op in this file assumes.
       :cljs (or (i64/bigint-value? form) (integer? form)))
    #?(:clj (long form) :cljs (i64/->bigint form))
    (string? form)
    (value/bounded-string! form value/string-literal-byte-limit)
    (value/f32-value? form) form
    (value/f64-value? form) form
    (keyword? form)
    (value/bounded-keyword! form value/keyword-value-byte-limit)
    (boolean? form) form
    (symbol? form) (if (contains? env form)
                     (get env form)
                     (trap! :unbound-symbol {:symbol form}))
    :else
    (let [[op & args] form]
      (cond
        ;; `let` body and `if` branch are tail positions -- rebind and loop,
        ;; never call back into this function. See the docstring.
        (= op 'let)
        (let [[bindings body] args
              env' (reduce (fn [e [name value]]
                             (assoc e name (eval-expr value e functions fuel heap call-stack cap-call)))
                           env (partition 2 bindings))]
          (recur body env'))

        (= op 'if)
        (let [[test then else] args
              test-value (eval-expr test env functions fuel heap call-stack cap-call)]
          (recur (if (kotoba-false? test-value) else then) env))

        (= op 'do)
        (last (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args))

        (= op 'cap-call)
        (let [[cap-id value] args]
          (when-not cap-call
            (trap! :capability-denied {:capability cap-id}))
          (let [result (cap-call cap-id (eval-expr value env functions fuel heap call-stack cap-call))]
            #?(:clj (long result) :cljs (i64/->bigint result))))

        (= op 'typed-cap-call)
        (let [[cap-id request-type result-type request-form] args]
          (when-not cap-call
            (trap! :capability-denied {:capability cap-id :typed true}))
          (let [request (eval-expr request-form env functions fuel heap call-stack cap-call)]
            (validate-runtime-value! request request-type
                                     {:capability cap-id :boundary :request})
            (validate-runtime-value! (cap-call cap-id request-type result-type request)
                                     result-type
                                     {:capability cap-id :boundary :result})))

        (= op 'pair)
        (let [[left right] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (allocate-pair! heap left right))

        (= op 'pair-first)
        (read-pair heap (eval-expr (first args) env functions fuel heap call-stack cap-call) 0)

        (= op 'pair-second)
        (read-pair heap (eval-expr (first args) env functions fuel heap call-stack cap-call) 1)

        (= op 'kgraph-assert!)
        (let [[e a v] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (assert-datom! heap e a v))

        (= op 'kgraph-get)
        (let [[e a] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (get-datom heap e a))

        (= op 'kgraph-count)
        (count-entities heap (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'kgraph-entity-at)
        (let [[a index] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (entity-at heap a index))

        (= op 'string-byte-length)
        (let [bytes (value/utf8-byte-count!
                     (eval-expr (first args) env functions fuel heap call-stack cap-call))]
          #?(:clj (long bytes) :cljs (i64/->bigint bytes)))

        ;; Product Value ABI v1: string-length is an alias of UTF-8 byte length
        ;; (indices for string-substring are also UTF-8 byte offsets).
        (= op 'string-length)
        (let [bytes (value/utf8-byte-count!
                     (eval-expr (first args) env functions fuel heap call-stack cap-call))]
          #?(:clj (long bytes) :cljs (i64/->bigint bytes)))

        ;; Decimal string of a signed i64 (kills hand-written digit recursion in pure oracles).
        (= op 'string-from-i64)
        (let [n (eval-expr (first args) env functions fuel heap call-stack cap-call)
              text #?(:clj (Long/toString (long n))
                      :cljs (.toString ^js n))]
          (value/bounded-string! text value/string-value-byte-limit))

        (= op 'bytes-empty)
        (value/empty-bytes)

        ;; Guest poll (ADR 0127): 1 if ready, 0 if pending; cancelled traps.
        (= op 'task-ready?)
        (let [task (eval-expr (first args) env functions fuel heap call-stack cap-call)
              polled (try
                       (value/task-poll task)
                       (catch #?(:clj Exception :cljs :default) error
                         (trap! :not-a-bytes-task {:message (ex-message error)})))]
          (case (:state polled)
            :ready #?(:clj 1 :cljs i64/one)
            :pending #?(:clj 0 :cljs i64/zero)
            :cancelled (trap! :task-cancelled {})
            (trap! :task-state-unknown {:state (:state polled)})))

        ;; Guest poll+read aggregate (ADR 0127): require ready task, drain the
        ;; stream, return total bytes, then linear-drop the task (ADR 0133).
        ;; Pending/cancelled/open-pending fail closed.
        (= op 'bytes-task-byte-count)
        (let [task (eval-expr (first args) env functions fuel heap call-stack cap-call)
              polled (try
                       (value/task-poll task)
                       (catch #?(:clj Exception :cljs :default) error
                         (trap! :not-a-bytes-task {:message (ex-message error)})))]
          (when-not (= :ready (:state polled))
            (trap! :task-not-ready {:state (:state polled)}))
          (let [stream (:stream polled)
                total
                (loop [acc 0]
                  (let [chunk (try
                                (value/stream-read! stream value/bytes-value-byte-limit)
                                (catch #?(:clj Exception :cljs :default) error
                                  (trap! :stream-read-failed {:message (ex-message error)})))]
                    (when (true? (:pending? chunk))
                      (trap! :stream-pending {:message "open stream has no chunk yet"}))
                    (let [n (+ acc (value/bytes-byte-count (:bytes chunk)))]
                      (if (:done? chunk)
                        n
                        (recur n)))))]
            (try
              (value/task-drop! task)
              (catch #?(:clj Exception :cljs :default) error
                (trap! :task-drop-failed {:message (ex-message error)})))
            #?(:clj (long total) :cljs (i64/->bigint total))))

        (= op 'string=?)
        (let [[left right] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (= left right))

        (= op 'string-concat)
        (let [[left right] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (value/bounded-string! (str left right) value/string-value-byte-limit))

        (= op 'string-substring)
        (let [[input start end]
              (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (value/utf8-substring! input start end))

        (= op 'string-code-point-at)
        (let [[input offset]
              (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)
              cp (value/utf8-code-point-at! input offset)]
          #?(:clj (long cp) :cljs (i64/->bigint cp)))

        (= op 'string-replace-all)
        (let [[input needle replacement]
              (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (when (empty? needle) (trap! :empty-string-replacement-needle {}))
          (value/bounded-string! (str/replace input needle replacement)
                                 value/string-value-byte-limit))

        (= op 'string-contains?)
        (let [[haystack needle]
              (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (when (empty? needle) (trap! :empty-string-search-needle {}))
          (str/includes? haystack needle))

        ;; T4.2: number of segments when splitting haystack by non-empty sep
        ;; (non-overlapping). Empty separator traps. Matches JS split length
        ;; for non-regex separators (e.g. "" / "," → 1, "a,b" / "," → 2).
        (= op 'string-split-count)
        (let [[haystack sep]
              (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (when (empty? sep) (trap! :empty-string-split-separator {}))
          (let [sep-len #?(:clj (count sep) :cljs (.-length sep))
                n (loop [i 0 acc 1]
                    (let [idx #?(:clj (.indexOf ^String haystack ^String sep i)
                                 :cljs (.indexOf haystack sep i))]
                      (if (neg? idx)
                        acc
                        (recur (+ idx sep-len) (inc acc)))))]
            #?(:clj (long n) :cljs (i64/->bigint n))))

        (= op 'string-fold-case)
        (value/bounded-string!
         (value/fold-case! (eval-expr (first args) env functions fuel heap call-stack cap-call))
         value/string-value-byte-limit)

        (= op 'keyword-from-string)
        (let [text (value/bounded-string!
                    (eval-expr (first args) env functions fuel heap call-stack cap-call)
                    value/keyword-value-byte-limit)]
          (when (or (empty? text) (= \: (first text))
                    (re-find #"[\s\[\]{}()\"',;`~^\\]" text))
            (trap! :invalid-keyword-source {}))
          (value/bounded-keyword! (keyword text) value/keyword-value-byte-limit))

        (= op 'keyword-name)
        (value/bounded-string!
         (name (eval-expr (first args) env functions fuel heap call-stack cap-call))
         value/string-value-byte-limit)

        (= op 'symbol)
        (let [text (value/bounded-string!
                    (eval-expr (first args) env functions fuel heap call-stack cap-call)
                    value/symbol-value-byte-limit)]
          (when (or (empty? text)
                    (re-find #"[\s\[\]{}()\"',;`~^\\]" text))
            (trap! :invalid-symbol-source {}))
          (value/bounded-symbol! (symbol text) value/symbol-value-byte-limit))

        (= op 'xml-path-count)
        (xml/path-count
         (eval-expr (first args) env functions fuel heap call-stack cap-call)
         (eval-expr (second args) env functions fuel heap call-stack cap-call))

        (= op 'xml-name-count)
        (xml/name-count
         (eval-expr (first args) env functions fuel heap call-stack cap-call)
         (eval-expr (second args) env functions fuel heap call-stack cap-call))

        (= op 'xml-name-text)
        (xml/name-text
         (eval-expr (nth args 0) env functions fuel heap call-stack cap-call)
         (eval-expr (nth args 1) env functions fuel heap call-stack cap-call)
         (eval-expr (nth args 2) env functions fuel heap call-stack cap-call))

        (= op 'xml-path-text)
        (xml/path-text
         (eval-expr (nth args 0) env functions fuel heap call-stack cap-call)
         (eval-expr (nth args 1) env functions fuel heap call-stack cap-call)
         (eval-expr (nth args 2) env functions fuel heap call-stack cap-call))

        (= op 'xml-path-attr)
        (xml/path-attr
         (eval-expr (nth args 0) env functions fuel heap call-stack cap-call)
         (eval-expr (nth args 1) env functions fuel heap call-stack cap-call)
         (eval-expr (nth args 2) env functions fuel heap call-stack cap-call)
         (eval-expr (nth args 3) env functions fuel heap call-stack cap-call))

        (= op 'decimal-f64-parse)
        (decimal/parse-f64
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'decimal-f64x3-parse)
        (decimal/parse-f64x3
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-to-bits)
        (value/f64-to-i64-bits
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-from-bits)
        (value/i64-bits-to-f64
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'i64-to-f64-checked)
        (value/i64-to-f64-checked
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'i64-to-f64-rounded)
        (value/i64-to-f64-rounded
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-to-i64-checked)
        (value/f64-to-i64-checked
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-to-i64-truncating)
        (value/f64-to-i64-truncating
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (contains? '#{f64-add f64-sub f64-mul f64-div f64-min f64-max} op)
        (let [[left right] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          ((case op f64-add + f64-sub - f64-mul * f64-div f64-divide
                 f64-min #?(:clj #(Math/min (double %1) (double %2)) :cljs js/Math.min)
                 f64-max #?(:clj #(Math/max (double %1) (double %2)) :cljs js/Math.max)) left right))

        (= op 'f64-neg)
        (- (double (eval-expr (first args) env functions fuel heap call-stack cap-call)))

        (= op 'f64-abs)
        (#?(:clj Math/abs :cljs js/Math.abs)
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-sqrt)
        (#?(:clj Math/sqrt :cljs js/Math.sqrt)
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-sin-quarter-turn)
        (f64-sin-quarter-turn
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-cos-quarter-turn)
        (f64-cos-quarter-turn
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-sin-bounded)
        (f64-sin-bounded
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-cos-bounded)
        (f64-cos-bounded
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-exp-near-zero)
        (f64-exp-near-zero
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-log-near-one)
        (f64-log-near-one
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-atan2-bounded)
        (let [[y x] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (f64-atan2-bounded y x))

        (= op 'f64-exp-bounded)
        (f64-exp-bounded
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-log-bounded)
        (f64-log-bounded
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (contains? '#{f64-eq f64-lt f64-le f64-gt f64-ge} op)
        (let [[left right] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          ((case op f64-eq = f64-lt < f64-le <= f64-gt > f64-ge >=) left right))

        (= op 'f64-unordered)
        (let [[left right] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (or #?(:clj (Double/isNaN ^double left) :cljs (js/Number.isNaN left))
              #?(:clj (Double/isNaN ^double right) :cljs (js/Number.isNaN right))))

        (= op 'f32-to-bits)
        (value/f32-to-i64-bits
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f32-from-bits)
        (value/i64-bits-to-f32
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f64-to-f32-rounded)
        (value/f64-to-f32-rounded
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f32-to-f64-exact)
        (value/f32-to-f64-exact
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'i64-to-f32-checked)
        (value/i64-to-f32-checked
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'i64-to-f32-rounded)
        (value/i64-to-f32-rounded
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f32-to-i64-checked)
        (value/f32-to-i64-checked
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'f32-to-i64-truncating)
        (value/f32-to-i64-truncating
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (contains? '#{f32-add f32-sub f32-mul f32-div f32-min f32-max} op)
        (let [[left right] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (as-f32 ((case op f32-add + f32-sub - f32-mul * f32-div f32-divide
                         f32-min #?(:clj #(Math/min (.floatValue ^Number %1) (.floatValue ^Number %2)) :cljs js/Math.min)
                         f32-max #?(:clj #(Math/max (.floatValue ^Number %1) (.floatValue ^Number %2)) :cljs js/Math.max)) left right)))

        (= op 'f32-neg)
        (as-f32 (- (eval-expr (first args) env functions fuel heap call-stack cap-call)))

        (= op 'f32-abs)
        (as-f32 (#?(:clj Math/abs :cljs js/Math.abs)
                 (eval-expr (first args) env functions fuel heap call-stack cap-call)))

        (= op 'f32-sqrt)
        (as-f32 (#?(:clj Math/sqrt :cljs js/Math.sqrt)
                 (eval-expr (first args) env functions fuel heap call-stack cap-call)))

        (contains? '#{f32-eq f32-lt f32-le f32-gt f32-ge} op)
        (let [[left right] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          ((case op f32-eq = f32-lt < f32-le <= f32-gt > f32-ge >=) left right))

        (= op 'f32-unordered)
        (let [[left right] (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (or #?(:clj (Float/isNaN ^float left) :cljs (js/Number.isNaN left))
              #?(:clj (Float/isNaN ^float right) :cljs (js/Number.isNaN right))))

        (= op 'map-new)
        (let [values (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)
              result (into (sorted-map) (map vec (partition 2 values)))]
          (when-not (= (quot (count values) 2) (count result))
            (trap! :duplicate-map-key {}))
          (value/bounded-map! result))

        (= op 'map-get)
        (let [[map-form key-form default-form] args
              map-value (eval-expr map-form env functions fuel heap call-stack cap-call)
              key-value (eval-expr key-form env functions fuel heap call-stack cap-call)]
          (value/bounded-map! map-value)
          (value/bounded-keyword! key-value value/keyword-value-byte-limit)
          (if (contains? map-value key-value)
            (get map-value key-value)
            (eval-expr default-form env functions fuel heap call-stack cap-call)))

        (= op 'map-assoc)
        (let [map-value (eval-expr (first args) env functions fuel heap call-stack cap-call)
              values (mapv #(eval-expr % env functions fuel heap call-stack cap-call)
                           (rest args))
              result (reduce (fn [current [key item]] (assoc current key item))
                             (value/bounded-map! map-value) (partition 2 values))]
          (value/bounded-map! result))

        (= op 'bool-not)
        (kotoba-false? (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'option-some)
        [true (eval-expr (first args) env functions fuel heap call-stack cap-call)]

        (= op 'option-none) [false]

        (= op 'option-some?)
        (let [option (value/bounded-option-i64!
                      (eval-expr (first args) env functions fuel heap call-stack cap-call))]
          (true? (first option)))

        (= op 'option-value)
        (let [[option-form fallback-form] args
              option (value/bounded-option-i64!
                      (eval-expr option-form env functions fuel heap call-stack cap-call))]
          (if (first option)
            (second option)
            (eval-expr fallback-form env functions fuel heap call-stack cap-call)))

        (= op 'result-ok)
        [true (eval-expr (first args) env functions fuel heap call-stack cap-call)]

        (= op 'result-err)
        [false (eval-expr (first args) env functions fuel heap call-stack cap-call)]

        (= op 'result-ok?)
        (let [result (value/bounded-result-i64!
                      (eval-expr (first args) env functions fuel heap call-stack cap-call))]
          (true? (first result)))

        (contains? '#{result-value result-error} op)
        (let [[result-form fallback-form] args
              result (value/bounded-result-i64!
                      (eval-expr result-form env functions fuel heap call-stack cap-call))
              selected? (if (= op 'result-value) (first result) (not (first result)))]
          (if selected?
            (second result)
            (eval-expr fallback-form env functions fuel heap call-stack cap-call)))

        (contains? '#{result-ok-of result-err-of} op)
        (let [[type payload-form] args
              tag (= op 'result-ok-of)]
          (value/bounded-typed-value!
           type [tag (eval-expr payload-form env functions fuel heap call-stack cap-call)]))

        (= op 'result-ok?-of)
        (let [[type result-form] args
              result (value/bounded-typed-value!
                      type (eval-expr result-form env functions fuel heap call-stack cap-call))]
          (true? (first result)))

        (contains? '#{result-value-of result-error-of} op)
        (let [[type result-form fallback-form] args
              result (value/bounded-typed-value!
                      type (eval-expr result-form env functions fuel heap call-stack cap-call))
              selected? (if (= op 'result-value-of) (first result) (not (first result)))
              payload-type (if (= op 'result-value-of) (second type) (nth type 2))]
          (if selected?
            (second result)
            (value/bounded-typed-value!
             payload-type
             (eval-expr fallback-form env functions fuel heap call-stack cap-call))))

        (= op 'result-match-of)
        (let [[type result-form ok-name ok-body err-name err-body] args
              result (value/bounded-typed-value!
                      type (eval-expr result-form env functions fuel heap call-stack cap-call))]
          (if (first result)
            (eval-expr ok-body (assoc env ok-name (second result))
                       functions fuel heap call-stack cap-call)
            (eval-expr err-body (assoc env err-name (second result))
                       functions fuel heap call-stack cap-call)))

        (= op 'variant-new)
        (let [[type tag payload-form] args]
          (value/bounded-typed-value!
           type [type tag (eval-expr payload-form env functions fuel heap call-stack cap-call)]))

        (= op 'variant-match)
        (let [[type value-form branches] args
              variant (value/bounded-typed-value!
                       type (eval-expr value-form env functions fuel heap call-stack cap-call))
              tag (second variant)
              [_ binder body] (some #(when (= tag (first %)) %) branches)]
          (when-not binder (trap! :unknown-variant-case {:tag tag}))
          (eval-expr body (assoc env binder (nth variant 2))
                     functions fuel heap call-stack cap-call))

        (= op 'option-some-of)
        (let [[type payload-form] args]
          (value/bounded-typed-value!
           type [type true (eval-expr payload-form env functions fuel heap call-stack cap-call)]))

        (= op 'option-none-of)
        (let [[type] args]
          (value/bounded-typed-value! type [type false]))

        (= op 'option-some?-of)
        (let [[type option-form] args
              option (value/bounded-typed-value!
                      type (eval-expr option-form env functions fuel heap call-stack cap-call))]
          (true? (second option)))

        (= op 'option-value-of)
        (let [[type option-form fallback-form] args
              option (value/bounded-typed-value!
                      type (eval-expr option-form env functions fuel heap call-stack cap-call))]
          (if (true? (second option))
            (nth option 2)
            (value/bounded-typed-value!
             (second type)
             (eval-expr fallback-form env functions fuel heap call-stack cap-call))))

        (= op 'option-match)
        (let [[type option-form none-body some-name some-body] args
              option (value/bounded-typed-value!
                      type (eval-expr option-form env functions fuel heap call-stack cap-call))]
          (if (true? (second option))
            (eval-expr some-body (assoc env some-name (nth option 2))
                       functions fuel heap call-stack cap-call)
            (eval-expr none-body env functions fuel heap call-stack cap-call)))

        (= op 'hetero-vector-new)
        (let [[type & item-forms] args
              items (mapv #(eval-expr % env functions fuel heap call-stack cap-call)
                          item-forms)]
          (value/bounded-typed-value! type (into [type] items)))

        (= op 'typed-list-new)
        (let [[type & item-forms] args
              items (mapv #(eval-expr % env functions fuel heap call-stack cap-call)
                          item-forms)]
          (value/bounded-typed-value! type [type items]))

        (= op 'hetero-vector-count)
        (let [[type value-form] args
              items (value/bounded-typed-value!
                     type (eval-expr value-form env functions fuel heap call-stack cap-call))]
          #?(:clj (long (dec (count items)))
             :cljs (i64/->bigint (dec (count items)))))

        (= op 'hetero-vector-at)
        (let [[type value-form index] args
              items (value/bounded-typed-value!
                     type (eval-expr value-form env functions fuel heap call-stack cap-call))
              host-index #?(:clj (long index) :cljs (js/Number index))]
          (nth items (inc host-index)))

        (= op 'hetero-vector-assoc)
        (let [[type value-form index item-form] args
              items (value/bounded-typed-value!
                     type (eval-expr value-form env functions fuel heap call-stack cap-call))
              item (eval-expr item-form env functions fuel heap call-stack cap-call)
              host-index #?(:clj (long index) :cljs (js/Number index))]
          (value/bounded-typed-value! type (assoc items (inc host-index) item)))

        (= op 'hetero-vector-equal)
        (let [[type left-form right-form] args
              left (value/bounded-typed-value!
                    type (eval-expr left-form env functions fuel heap call-stack cap-call))
              right (value/bounded-typed-value!
                     type (eval-expr right-form env functions fuel heap call-stack cap-call))]
          #?(:clj (if (= left right) 1 0)
             :cljs (if (= left right) i64/one i64/zero)))

        (= op 'typed-set-new)
        (let [[type & item-forms] args
              items (mapv #(eval-expr % env functions fuel heap call-stack cap-call)
                          item-forms)]
          (value/bounded-typed-value! type [type items]))

        (= op 'typed-set-count)
        (let [[type value-form] args
              set-value (value/bounded-typed-value!
                         type (eval-expr value-form env functions fuel heap call-stack cap-call))]
          #?(:clj (long (count (second set-value)))
             :cljs (i64/->bigint (count (second set-value)))))

        (= op 'typed-set-contains)
        (let [[type value-form item-form] args
              set-value (value/bounded-typed-value!
                         type (eval-expr value-form env functions fuel heap call-stack cap-call))
              item (value/bounded-typed-value!
                    (second type)
                    (eval-expr item-form env functions fuel heap call-stack cap-call))]
          (boolean (some #(zero? (value/compare-typed-values (second type) % item))
                         (second set-value))))

        (= op 'typed-set-conj)
        (let [[type value-form item-form] args
              set-value (value/bounded-typed-value!
                         type (eval-expr value-form env functions fuel heap call-stack cap-call))
              item (value/bounded-typed-value!
                    (second type)
                    (eval-expr item-form env functions fuel heap call-stack cap-call))]
          (if (some #(zero? (value/compare-typed-values (second type) % item))
                    (second set-value))
            set-value
            (do (when (>= (count (second set-value)) value/typed-set-item-limit)
                  (trap! :set-too-large {:limit value/typed-set-item-limit}))
                (value/bounded-typed-value!
                 type [type (conj (second set-value) item)]))))

        (= op 'typed-set-disj)
        (let [[type value-form item-form] args
              set-value (value/bounded-typed-value!
                         type (eval-expr value-form env functions fuel heap call-stack cap-call))
              item (value/bounded-typed-value!
                    (second type)
                    (eval-expr item-form env functions fuel heap call-stack cap-call))]
          (value/bounded-typed-value!
           type [type (filterv #(not (zero? (value/compare-typed-values
                                             (second type) % item)))
                               (second set-value))]))

        (= op 'typed-set-equal)
        (let [[type left-form right-form] args
              left (value/bounded-typed-value!
                    type (eval-expr left-form env functions fuel heap call-stack cap-call))
              right (value/bounded-typed-value!
                     type (eval-expr right-form env functions fuel heap call-stack cap-call))]
          #?(:clj (if (= left right) 1 0)
             :cljs (if (= left right) i64/one i64/zero)))

        (= op 'typed-set-nth)
        (let [[type value-form index-form] args
              set-value (value/bounded-typed-value!
                         type (eval-expr value-form env functions fuel heap call-stack cap-call))
              raw-index (eval-expr index-form env functions fuel heap call-stack cap-call)
              index #?(:clj (long raw-index) :cljs (js/Number raw-index))
              items (second set-value)]
          (when (or (neg? index) (>= index (count items)))
            (trap! :set-index-out-of-bounds {:index index :count (count items)}))
          (value/bounded-typed-value! (second type) (nth items index)))

        (= op 'typed-map-new)
        (let [[type & entry-forms] args
              evaluated (mapv #(eval-expr % env functions fuel heap call-stack cap-call)
                              entry-forms)
              entries (mapv vec (partition 2 evaluated))]
          (value/bounded-typed-value! type [type entries]))

        (= op 'typed-map-count)
        (let [[type value-form] args
              map-value (value/bounded-typed-value!
                         type (eval-expr value-form env functions fuel heap call-stack cap-call))]
          #?(:clj (long (count (second map-value)))
             :cljs (i64/->bigint (count (second map-value)))))

        (contains? '#{typed-map-contains typed-map-get typed-map-dissoc} op)
        (let [[type value-form key-form] args
              map-value (value/bounded-typed-value!
                         type (eval-expr value-form env functions fuel heap call-stack cap-call))
              key (value/bounded-typed-value!
                   (second type)
                   (eval-expr key-form env functions fuel heap call-stack cap-call))
              match (some (fn [[candidate item]]
                            (when (zero? (value/compare-typed-values
                                          (second type) candidate key))
                              [candidate item]))
                          (second map-value))]
          (case op
            typed-map-contains (boolean match)
            typed-map-get (let [option-type [:option (nth type 2)]]
                            (if match [option-type true (second match)]
                                [option-type false]))
            typed-map-dissoc
            (value/bounded-typed-value!
             type [type (filterv (fn [[candidate _]]
                                   (not (zero? (value/compare-typed-values
                                                (second type) candidate key))))
                                 (second map-value))])))

        (= op 'typed-map-entry-at)
        (let [[type value-form index-form] args
              map-value (value/bounded-typed-value!
                         type (eval-expr value-form env functions fuel heap call-stack cap-call))
              raw-index (eval-expr index-form env functions fuel heap call-stack cap-call)
              index #?(:clj (long raw-index) :cljs (js/Number raw-index))
              entry-type [:vector [(second type) (nth type 2)]]
              option-type [:option entry-type]]
          (if (or (neg? index) (>= index (count (second map-value))))
            [option-type false]
            (let [[key item] (nth (second map-value) index)]
              [option-type true [entry-type key item]])))

        (= op 'typed-map-assoc)
        (let [[type value-form key-form item-form] args
              map-value (value/bounded-typed-value!
                         type (eval-expr value-form env functions fuel heap call-stack cap-call))
              key (value/bounded-typed-value!
                   (second type) (eval-expr key-form env functions fuel heap call-stack cap-call))
              item (value/bounded-typed-value!
                    (nth type 2) (eval-expr item-form env functions fuel heap call-stack cap-call))
              remaining (filterv (fn [[candidate _]]
                                   (not (zero? (value/compare-typed-values
                                                (second type) candidate key))))
                                 (second map-value))]
          (when (and (= (count remaining) (count (second map-value)))
                     (>= (count remaining) value/typed-map-entry-limit))
            (trap! :map-too-large {:limit value/typed-map-entry-limit}))
          (value/bounded-typed-value! type [type (conj remaining [key item])]))

        (= op 'typed-map-equal)
        (let [[type left-form right-form] args
              left (value/bounded-typed-value!
                    type (eval-expr left-form env functions fuel heap call-stack cap-call))
              right (value/bounded-typed-value!
                     type (eval-expr right-form env functions fuel heap call-stack cap-call))]
          #?(:clj (if (= left right) 1 0)
             :cljs (if (= left right) i64/one i64/zero)))

        (= op 'record-new)
        (let [[type & value-forms] args
              values (mapv #(eval-expr % env functions fuel heap call-stack cap-call)
                           value-forms)]
          (value/bounded-typed-value! type (into [type] values)))

        (= op 'record-get)
        (let [[type value-form field] args
              record-value (value/bounded-typed-value!
                            type (eval-expr value-form env functions fuel heap call-stack cap-call))
              field-index (first (keep-indexed (fn [index [declared-field _]]
                                                 (when (= declared-field field) index))
                                               (nth type 2)))]
          (when (nil? field-index) (trap! :unknown-record-field {:field field}))
          (nth record-value (inc field-index)))

        (= op 'record-assoc)
        (let [[type value-form field replacement-form] args
              record-value (value/bounded-typed-value!
                            type (eval-expr value-form env functions fuel heap call-stack cap-call))
              replacement (eval-expr replacement-form env functions fuel heap call-stack cap-call)
              field-index (first (keep-indexed (fn [index [declared-field _]]
                                                 (when (= declared-field field) index))
                                               (nth type 2)))]
          (when (nil? field-index) (trap! :unknown-record-field {:field field}))
          (value/bounded-typed-value! type
                                      (assoc record-value (inc field-index) replacement)))

        (= op 'record-equal)
        (let [[type left-form right-form] args
              left (value/bounded-typed-value!
                    type (eval-expr left-form env functions fuel heap call-stack cap-call))
              right (value/bounded-typed-value!
                     type (eval-expr right-form env functions fuel heap call-stack cap-call))]
          #?(:clj (if (= left right) 1 0)
             :cljs (if (= left right) i64/one i64/zero)))

        (= op 'vector-new)
        (value/bounded-vector-i64!
         (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args))

        (= op 'vector-count)
        (let [value (eval-expr (first args) env functions fuel heap call-stack cap-call)
              items (if (and (vector? value) (= 2 (count value))
                             (vector? (first value)) (= :list (ffirst value)))
                      (second (value/bounded-typed-value! (first value) value))
                      (value/bounded-vector-i64! value))]
          #?(:clj (long (count items)) :cljs (i64/->bigint (count items))))

        (= op 'vector-get)
        (let [[items-form index-form fallback-form] args
              items (value/bounded-vector-i64!
                     (eval-expr items-form env functions fuel heap call-stack cap-call))
              index (eval-expr index-form env functions fuel heap call-stack cap-call)]
          (if (and #?(:clj (integer? index) :cljs (i64/bigint-value? index))
                   (not (neg? index)) (< index (count items)))
            (nth items #?(:clj index :cljs (js/Number index)))
            (eval-expr fallback-form env functions fuel heap call-stack cap-call)))

        (= op 'vector-at)
        (let [[items-form index-form] args
              items (value/bounded-vector-i64!
                     (eval-expr items-form env functions fuel heap call-stack cap-call))
              index (eval-expr index-form env functions fuel heap call-stack cap-call)]
          (when-not (and (not (neg? index)) (< index (count items)))
            (trap! :vector-index-out-of-range {:index index}))
          (nth items #?(:clj index :cljs (js/Number index))))

        (= op 'vector-drop)
        (let [[items-form count-form] args
              items (value/bounded-vector-i64!
                     (eval-expr items-form env functions fuel heap call-stack cap-call))
              drop-count (eval-expr count-form env functions fuel heap call-stack cap-call)]
          (when-not (and (not (neg? drop-count)) (<= drop-count (count items)))
            (trap! :vector-drop-out-of-range {:count drop-count}))
          (value/bounded-vector-i64!
           (subvec items #?(:clj drop-count :cljs (js/Number drop-count)))))

        ;; A vector of `n` zeros.
        ;;
        ;; `vector-new` is variadic, so building a million-slot struct of
        ;; arrays through it would need a million arguments in source -- and
        ;; the literal limit refuses that long before the item limit does,
        ;; which is correct: nobody writes a book as a literal. This is the
        ;; allocation `torihiki.slab/alloc` is, expressed once.
        (= op 'vector-alloc)
        (let [n (eval-expr (first args) env functions fuel heap call-stack cap-call)]
          (when (or (neg? n) (> n value/vector-item-limit))
            (trap! :vector-alloc-out-of-range {:count n}))
          ;; The zero is per-runtime: on ClojureScript an i64 item must be a
          ;; BigInt (`i64/bigint-value?`), and a plain 0 is refused with
          ;; "vector item is not a signed i64" -- which is what a million-slot
          ;; allocation answered the first time it was compiled. The JVM half
          ;; was fine, so only the runtime that deploys was broken.
          (value/bounded-vector-i64!
           (vec (repeat #?(:clj n :cljs (js/Number n))
                        #?(:clj 0 :cljs (js/BigInt 0))))))

        ;; `vector-assoc!` is the SAME operation here, deliberately.
        ;;
        ;; The bang says the caller has proved the handle is dead afterwards,
        ;; which lets a backend lower the update to a store instead of a copy.
        ;; That is a lowering, not a semantics: if the old handle is really
        ;; dead, in-place and copy are indistinguishable to every observer. So
        ;; the reference interpreter -- the thing other backends are checked
        ;; against -- must not distinguish them either, or the bang would
        ;; change what a program means and the whole argument for admitting it
        ;; would be false.
        (contains? '#{vector-assoc vector-assoc!} op)
        (let [[items-form index-form item-form] args
              items (value/bounded-vector-i64!
                     (eval-expr items-form env functions fuel heap call-stack cap-call))
              index (eval-expr index-form env functions fuel heap call-stack cap-call)
              item (eval-expr item-form env functions fuel heap call-stack cap-call)]
          (when-not (and (not (neg? index)) (< index (count items)))
            (trap! :vector-index-out-of-range {:index index}))
          (value/bounded-vector-i64!
           (assoc items #?(:clj index :cljs (js/Number index)) item)))

        (= op 'vector-conj)
        (let [[items-form item-form] args
              items (value/bounded-vector-i64!
                     (eval-expr items-form env functions fuel heap call-stack cap-call))
              item (eval-expr item-form env functions fuel heap call-stack cap-call)]
          (when (>= (count items) value/vector-item-limit)
            (trap! :vector-too-large {:limit value/vector-item-limit}))
          (value/bounded-vector-i64! (conj items item)))

        (= op 'vector-f64-new)
        (value/bounded-vector-f64!
         (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args))

        (= op 'vector-f64-count)
        (let [items (value/bounded-vector-f64!
                     (eval-expr (first args) env functions fuel heap call-stack cap-call))]
          #?(:clj (long (count items)) :cljs (i64/->bigint (count items))))

        (= op 'vector-f64-get)
        (let [[items-form index-form fallback-form] args
              items (value/bounded-vector-f64!
                     (eval-expr items-form env functions fuel heap call-stack cap-call))
              index (eval-expr index-form env functions fuel heap call-stack cap-call)]
          (if (and #?(:clj (integer? index) :cljs (i64/bigint-value? index))
                   (not (neg? index)) (< index (count items)))
            (nth items #?(:clj index :cljs (js/Number index)))
            (value/bounded-typed-value!
             :f64 (eval-expr fallback-form env functions fuel heap call-stack cap-call))))

        (= op 'vector-f64-at)
        (let [[items-form index-form] args
              items (value/bounded-vector-f64!
                     (eval-expr items-form env functions fuel heap call-stack cap-call))
              index (eval-expr index-form env functions fuel heap call-stack cap-call)]
          (when-not (and #?(:clj (integer? index) :cljs (i64/bigint-value? index))
                         (not (neg? index)) (< index (count items)))
            (trap! :vector-f64-index-out-of-range {:index index}))
          (nth items #?(:clj index :cljs (js/Number index))))

        (= op 'vector-f64-drop)
        (let [[items-form count-form] args
              items (value/bounded-vector-f64!
                     (eval-expr items-form env functions fuel heap call-stack cap-call))
              drop-count (eval-expr count-form env functions fuel heap call-stack cap-call)]
          (when-not (and #?(:clj (integer? drop-count) :cljs (i64/bigint-value? drop-count))
                         (not (neg? drop-count)) (<= drop-count (count items)))
            (trap! :vector-f64-drop-out-of-range {:count drop-count}))
          (value/bounded-vector-f64!
           (subvec items #?(:clj drop-count :cljs (js/Number drop-count)))))

        (= op 'vector-f64-assoc)
        (let [[items-form index-form item-form] args
              items (value/bounded-vector-f64!
                     (eval-expr items-form env functions fuel heap call-stack cap-call))
              index (eval-expr index-form env functions fuel heap call-stack cap-call)
              item (value/bounded-typed-value!
                    :f64 (eval-expr item-form env functions fuel heap call-stack cap-call))]
          (when-not (and #?(:clj (integer? index) :cljs (i64/bigint-value? index))
                         (not (neg? index)) (< index (count items)))
            (trap! :vector-f64-index-out-of-range {:index index}))
          (value/bounded-vector-f64!
           (assoc items #?(:clj index :cljs (js/Number index)) item)))

        (= op 'vector-f64-conj)
        (let [[items-form item-form] args
              items (value/bounded-vector-f64!
                     (eval-expr items-form env functions fuel heap call-stack cap-call))
              item (value/bounded-typed-value!
                    :f64 (eval-expr item-form env functions fuel heap call-stack cap-call))]
          (when (>= (count items) value/vector-item-limit)
            (trap! :vector-f64-too-large {:limit value/vector-item-limit}))
          (value/bounded-vector-f64! (conj items item)))

        (= op 'string-index-new) []

        (= op 'string-index-count)
        (let [index (value/bounded-string-index!
                     (eval-expr (first args) env functions fuel heap call-stack cap-call))]
          #?(:clj (long (count index)) :cljs (i64/->bigint (count index))))

        (contains? '#{string-index-contains string-index-get} op)
        (let [[index-form key-form] args
              index (value/bounded-string-index!
                     (eval-expr index-form env functions fuel heap call-stack cap-call))
              key (value/bounded-typed-value!
                   :string (eval-expr key-form env functions fuel heap call-stack cap-call))
              found (some (fn [[candidate item]] (when (= candidate key) item)) index)]
          (if (= op 'string-index-contains)
            (boolean (some? found))
            (if (some? found) [[:option :i64] true found] [[:option :i64] false])))

        (= op 'string-index-assoc)
        (let [[index-form key-form item-form] args
              index (value/bounded-string-index!
                     (eval-expr index-form env functions fuel heap call-stack cap-call))
              key (value/bounded-typed-value!
                   :string (eval-expr key-form env functions fuel heap call-stack cap-call))
              item (value/bounded-typed-value!
                    :i64 (eval-expr item-form env functions fuel heap call-stack cap-call))
              without-key (filterv #(not= key (first %)) index)]
          (when (and (= (count without-key) (count index))
                     (>= (count index) value/compact-graph-item-limit))
            (trap! :string-index-too-large {:limit value/compact-graph-item-limit}))
          (value/bounded-string-index! (vec (sort-by first (conj without-key [key item])))))

        (= op 'disjoint-set-i64-new)
        (let [size-value (eval-expr (first args) env functions fuel heap call-stack cap-call)]
          (when-not (and #?(:clj (integer? size-value) :cljs (i64/bigint-value? size-value))
                         (<= 0 size-value value/compact-graph-item-limit))
            (trap! :disjoint-set-i64-size-out-of-range
                   {:limit value/compact-graph-item-limit}))
          (let [size #?(:clj (int size-value) :cljs (js/Number size-value))
                parents #?(:clj (mapv long (range size))
                           :cljs (mapv i64/->bigint (range size)))
                ranks (vec (repeat size #?(:clj 0 :cljs i64/zero)))]
            (value/bounded-disjoint-set-i64! [parents ranks])))

        (= op 'disjoint-set-i64-count)
        (let [[parents _] (value/bounded-disjoint-set-i64!
                           (eval-expr (first args) env functions fuel heap call-stack cap-call))]
          #?(:clj (long (count parents)) :cljs (i64/->bigint (count parents))))

        (= op 'disjoint-set-i64-union)
        (let [[set-form left-form right-form] args
              [parents ranks :as disjoint-set]
              (value/bounded-disjoint-set-i64!
               (eval-expr set-form env functions fuel heap call-stack cap-call))
              left-index (compact-host-index
                          (eval-expr left-form env functions fuel heap call-stack cap-call)
                          (count parents) :disjoint-set-i64-index-out-of-range)
              right-index (compact-host-index
                           (eval-expr right-form env functions fuel heap call-stack cap-call)
                           (count parents) :disjoint-set-i64-index-out-of-range)
              left-root (disjoint-root parents left-index)
              right-root (disjoint-root parents right-index)
              option-type [:option :disjoint-set-i64]]
          (if (= left-root right-root)
            [option-type false]
            (let [left-rank (nth ranks left-root)
                  right-rank (nth ranks right-root)
                  [child root equal-rank?]
                  (cond (< left-rank right-rank) [left-root right-root false]
                        (> left-rank right-rank) [right-root left-root false]
                        :else [right-root left-root true])
                  new-parents (assoc parents child #?(:clj (long root) :cljs (i64/->bigint root)))
                  new-ranks (if equal-rank?
                              (assoc ranks root #?(:clj (inc (long left-rank))
                                                   :cljs (+ left-rank i64/one)))
                              ranks)]
              [option-type true
               (value/bounded-disjoint-set-i64! [new-parents new-ranks])])))

        (= op 'document-null) ["null"]

        (contains? '#{document-bool document-i64 document-f64
                      document-string document-keyword document-symbol} op)
        (let [type (case op document-bool :bool document-i64 :i64 document-f64 :f64
                         document-string :string document-keyword :keyword document-symbol :symbol)
              tag (name type)
              item (value/bounded-typed-value!
                    type (eval-expr (first args) env functions fuel heap call-stack cap-call))]
          (value/bounded-document! [tag item]))

        (= op 'document-vector)
        (value/bounded-document!
         ["vector" (mapv #(value/bounded-document!
                            (eval-expr % env functions fuel heap call-stack cap-call)) args)])

        (= op 'document-list)
        (value/bounded-document!
         ["list" (mapv #(value/bounded-document!
                          (eval-expr % env functions fuel heap call-stack cap-call)) args)])

        (= op 'document-set)
        (let [items (mapv #(value/bounded-document!
                            (eval-expr % env functions fuel heap call-stack cap-call)) args)
              canonical (vec (sort value/document-compare items))]
          (when-not (every? (fn [[left right]]
                              (neg? (value/document-compare left right)))
                            (partition 2 1 canonical))
            (trap! :duplicate-document-set-item {}))
          (value/bounded-document! ["set" canonical]))

        (= op 'document-map)
        (let [entries (mapv (fn [[key-form item-form]]
                              [(let [key (eval-expr key-form env functions fuel heap call-stack cap-call)]
                                 (if (keyword? key)
                                   ["keyword" (value/bounded-typed-value! :keyword key)]
                                   (value/bounded-document! key)))
                               (value/bounded-document!
                                (eval-expr item-form env functions fuel heap call-stack cap-call))])
                            (partition 2 args))]
          (value/bounded-document!
           ["map" (vec (sort (fn [[left] [right]] (value/document-map-key-compare left right)) entries))]))

        (= op 'document-count)
        (let [[tag payload] (value/bounded-document!
                             (eval-expr (first args) env functions fuel heap call-stack cap-call))]
          (when-not (contains? #{"map" "vector" "list" "set"} tag)
            (trap! :document-container-required {:tag tag}))
          #?(:clj (long (count payload)) :cljs (i64/->bigint (count payload))))

        (= op 'document-set-contains?)
        (let [[tag items]
              (value/bounded-document!
               (eval-expr (first args) env functions fuel heap call-stack cap-call))
              item (value/bounded-document!
                    (eval-expr (second args) env functions fuel heap call-stack cap-call))]
          (when-not (= "set" tag) (trap! :document-set-required {:tag tag}))
          (boolean (some #(zero? (value/document-compare % item)) items)))

        (contains? '#{document-vector-at document-list-at document-vector-assoc
                      document-vector-conj document-vector-drop document-vector-remove} op)
        (let [[document-form index-or-item-form item-form] args
              [tag items]
              (value/bounded-document!
               (eval-expr document-form env functions fuel heap call-stack cap-call))
              expected-tag (if (= op 'document-list-at) "list" "vector")
              _ (when-not (= expected-tag tag)
                  (trap! (if (= expected-tag "list") :document-list-required :document-vector-required)
                         {:tag tag}))]
          (case op
            document-vector-at
            (let [index (value/bounded-typed-value!
                         :i64 (eval-expr index-or-item-form env functions fuel heap call-stack cap-call))]
              (if (and (not (neg? index)) (< index (count items)))
                [[:option :document] true (nth items (container-host-index index))]
                [[:option :document] false]))
            document-list-at
            (let [index (value/bounded-typed-value!
                         :i64 (eval-expr index-or-item-form env functions fuel heap call-stack cap-call))]
              (if (and (not (neg? index)) (< index (count items)))
                [[:option :document] true (nth items (container-host-index index))]
                [[:option :document] false]))
            document-vector-assoc
            (let [index (value/bounded-typed-value!
                         :i64 (eval-expr index-or-item-form env functions fuel heap call-stack cap-call))
                  item (value/bounded-document!
                        (eval-expr item-form env functions fuel heap call-stack cap-call))]
              (when-not (and (not (neg? index)) (< index (count items)))
                (trap! :document-vector-index-out-of-range {:index index :count (count items)}))
              (value/bounded-document!
               ["vector" (assoc items (container-host-index index) item)]))
            document-vector-conj
            (let [item (value/bounded-document!
                        (eval-expr index-or-item-form env functions fuel heap call-stack cap-call))]
              (when (>= (count items) value/document-container-item-limit)
                (trap! :document-vector-too-large {:limit value/document-container-item-limit}))
              (value/bounded-document! ["vector" (conj items item)]))
            document-vector-drop
            (let [drop-count (value/bounded-typed-value!
                              :i64 (eval-expr index-or-item-form env functions fuel heap call-stack cap-call))]
              (when-not (and (not (neg? drop-count)) (<= drop-count (count items)))
                (trap! :document-vector-drop-out-of-range
                       {:count drop-count :items (count items)}))
              (value/bounded-document!
               ["vector" (subvec items (container-host-index drop-count))]))
            document-vector-remove
            (let [index (value/bounded-typed-value!
                         :i64 (eval-expr index-or-item-form env functions fuel heap call-stack cap-call))]
              (when-not (and (not (neg? index)) (< index (count items)))
                (trap! :document-vector-index-out-of-range {:index index :count (count items)}))
              (let [at (container-host-index index)]
                (value/bounded-document!
                 ["vector" (vec (concat (subvec items 0 at)
                                        (subvec items (inc at))))])))))

        (= op 'document-map-entry-at)
        (let [[tag entries]
              (value/bounded-document!
               (eval-expr (first args) env functions fuel heap call-stack cap-call))
              index (value/bounded-typed-value!
                     :i64 (eval-expr (second args) env functions fuel heap call-stack cap-call))]
          (when-not (= "map" tag) (trap! :document-map-required {:tag tag}))
          (if (and (not (neg? index)) (< index (count entries)))
            (let [[key item] (nth entries (container-host-index index))]
              [[:option :document] true
               (value/bounded-document! ["vector" [key item]])])
            [[:option :document] false]))

        (= op 'document-kind)
        (let [[tag]
              (value/bounded-document!
               (eval-expr (first args) env functions fuel heap call-stack cap-call))]
          (value/bounded-keyword! (keyword tag) value/keyword-value-byte-limit))

        (= op 'document-equal?)
        (let [[left right]
              (mapv #(value/bounded-document!
                      (eval-expr % env functions fuel heap call-stack cap-call)) args)]
          (= left right))

        (= op 'document-sha256)
        (value/document-sha256-hex
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'document-print)
        (value/document-print
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'document-read)
        (value/document-read
         (value/bounded-string!
          (eval-expr (first args) env functions fuel heap call-stack cap-call)
          value/string-value-byte-limit))

        (= op 'document-edn-print)
        (value/document-edn-print
         (eval-expr (first args) env functions fuel heap call-stack cap-call))

        (= op 'document-edn-read)
        (value/document-edn-read
         (value/bounded-string!
          (eval-expr (first args) env functions fuel heap call-stack cap-call)
          value/string-value-byte-limit))

        (contains? '#{document-contains document-get document-assoc document-dissoc} op)
        (let [[document-form key-form item-form] args
              [tag entries :as document]
              (value/bounded-document!
               (eval-expr document-form env functions fuel heap call-stack cap-call))
              _ (when-not (= "map" tag) (trap! :document-map-required {:tag tag}))
              raw-key (eval-expr key-form env functions fuel heap call-stack cap-call)
              key (if (keyword? raw-key)
                    ["keyword" (value/bounded-typed-value! :keyword raw-key)]
                    (value/bounded-document! raw-key))
              position (first (keep-indexed #(when (zero? (value/document-map-key-compare key (first %2))) %1)
                                            entries))]
          (case op
            document-contains (boolean (some? position))
            document-get (if (some? position)
                           [[:option :document] true (second (nth entries position))]
                           [[:option :document] false])
            document-dissoc (if (some? position)
                              (value/bounded-document!
                               ["map" (vec (concat (subvec entries 0 position)
                                                    (subvec entries (inc position))))])
                              document)
            document-assoc
            (let [item (value/bounded-document!
                        (eval-expr item-form env functions fuel heap call-stack cap-call))
                  output (if (some? position)
                           (assoc entries position [key item])
                           (conj entries [key item]))]
              (when (> (count output) value/document-container-item-limit)
                (trap! :document-map-too-large
                       {:limit value/document-container-item-limit}))
              (value/bounded-document!
               ["map" (vec (sort (fn [[left] [right]]
                                    (value/document-map-key-compare left right)) output))]))))

        (= op 'document-merge)
        (let [documents (mapv #(value/bounded-document!
                                (eval-expr % env functions fuel heap call-stack cap-call)) args)
              _ (doseq [[tag _] documents]
                  (when-not (= "map" tag) (trap! :document-map-required {:tag tag})))
              entries (reduce (fn [result [key item]]
                                (let [position (first
                                                (keep-indexed
                                                 #(when (zero? (value/document-map-key-compare key (first %2))) %1)
                                                 result))]
                                  (if (some? position)
                                    (assoc result position [key item])
                                    (conj result [key item]))))
                              [] (mapcat second documents))
              entries (vec (sort (fn [[left] [right]]
                                   (value/document-map-key-compare left right)) entries))]
          (when (> (count entries) value/document-container-item-limit)
            (trap! :document-map-too-large {:limit value/document-container-item-limit}))
          (value/bounded-document! ["map" (mapv vec entries)]))

        (contains? '#{document-string-value document-keyword-value document-symbol-value document-bool-value
                      document-i64-value document-f64-value} op)
        (let [[tag payload] (value/bounded-document!
                             (eval-expr (first args) env functions fuel heap call-stack cap-call))
              type (case op document-string-value :string document-keyword-value :keyword document-symbol-value :symbol document-bool-value :bool
                         document-i64-value :i64 document-f64-value :f64)
              option-type [:option type]]
          (if (= tag (name type)) [option-type true payload] [option-type false]))

        ;; The loads, the stores, `kernel-subregion` and the lock pair. With no
        ;; `:memory` every one of them refuses exactly as before.
        ;;
        ;; The lock pair used to refuse EVEN WITH an image, and the reason
        ;; deserves its epitaph: their value is whether this caller won a race,
        ;; so answering was said to invent "the lock was free" from a compiler
        ;; that had never seen the memory. That was right while there was
        ;; nothing to consult. It stopped being right when the caller began
        ;; supplying the word: `kernel-try-lock-u32` is a compare-and-swap
        ;; against a comparand and a replacement THE OPERATION fixes (0 -> 1,
        ;; and 1 -> 0 for unlock), so on bytes the caller wrote, in an
        ;; interpreter with one thread, the answer is determined rather than
        ;; invented.
        ;;
        ;; The boundary, which any receipt built on this has to carry: it
        ;; models the UNCONTENDED case and nothing else. A vector that takes
        ;; the lock and gets 1 has shown that the object takes a free lock. It
        ;; has not shown anything whatsoever about two callers, because there
        ;; is only ever one here.
        (contains? '#{kernel-load-u8 kernel-load-u8-4k kernel-load-u8-16k
                      kernel-store-u8 kernel-store-u8-4k kernel-subregion
                      kernel-load-u32 kernel-store-u32
                      kernel-try-lock-u32 kernel-unlock-u32} op)
        (if-let [memory (:memory heap)]
          (kernel-memory-call!
           op memory
           (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args))
          (trap! :kernel-memory-unavailable {:operation op}))

        ;; `kernel-in-u8`/`kernel-in-u32` (x86 port reads) belong here for the
        ;; same reason their write counterparts do, and one more: their VALUE
        ;; is whatever a device put on the bus. There is no answer this
        ;; interpreter could return that would be right, so it must refuse
        ;; rather than invent one -- the alternative would let the oracle
        ;; "confirm" a device read off-hardware.
        ;; `kernel-read-msr`/`kernel-write-msr` for the same reason again. An
        ;; MSR read is not a bus transaction, so it is tempting to think it
        ;; could be modelled -- but its value is CPU and firmware state
        ;; (EFER, the APIC base, the SYSCALL entry point), often written by
        ;; another core, and `rdmsr` faults outside ring 0 regardless. There
        ;; is nothing here that could be right.
        ;; `kernel-cpuid-*` is the sharpest case of all. A `cpuid` result is a
        ;; property of the MACHINE -- which CPU this is, and what it can do --
        ;; and this interpreter is not running on the machine the artifact will
        ;; run on. Answering would not merely invent a value: the six aiueos
        ;; sites BRANCH on it, so an invented answer becomes "this CPU supports
        ;; NX" decided by a compiler that has never seen the CPU. It refuses.
        (contains? '#{kernel-boot-info kernel-read-cr0 kernel-write-cr0
                      kernel-read-cr2 kernel-read-cr3 kernel-write-cr3 kernel-invlpg
                      kernel-read-cs kernel-page-fault-handler-address
                      kernel-rt-timer-handler-address
                      kernel-page-fault-recovery-handler-address
                      kernel-configure-page-fault-recovery kernel-load-idt
                      kernel-double-fault-handler-address
                      kernel-configure-double-fault-ist kernel-load-gdt-tss
                      kernel-probe-guard-write kernel-probe-text-write kernel-probe-nx-execute
                      kernel-probe-recoverable-guard-write kernel-probe-double-fault
                      kernel-cli kernel-sti kernel-hlt kernel-pause
                      kernel-out-u8 kernel-out-u32
                      kernel-in-u8 kernel-in-u32
                      kernel-read-msr kernel-write-msr
                      kernel-cpuid-eax kernel-cpuid-ebx
                      kernel-cpuid-ecx kernel-cpuid-edx} op)
        (trap! :kernel-privileged-unavailable {:operation op})

        (contains? '#{+ - * quot bit-xor bit-and bit-or = < > <= >=} op)
        (let [xs (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          #?(:clj
             (case op
               + (reduce i64-add xs)
               - (if (= 1 (count xs)) (i64-sub 0 (first xs)) (reduce i64-sub xs))
               * (reduce i64-mul xs)
               quot (let [[x y] xs]
                      (when (zero? y) (trap! :division-by-zero {}))
                      (when (and (= x Long/MIN_VALUE) (= y -1))
                        (trap! :signed-division-overflow {}))
                      (quot x y))
               bit-xor (apply bit-xor xs)
               bit-and (apply bit-and xs)
               bit-or (apply bit-or xs)
               = (if (apply = xs) 1 0)
               < (if (apply < xs) 1 0)
               > (if (apply > xs) 1 0)
               <= (if (apply <= xs) 1 0)
               >= (if (apply >= xs) 1 0))
             :cljs
             ;; `xs` are always bigint already (every literal/sub-expression
             ;; passed through the coercion above), so plain `bit-xor`/
             ;; `bit-and`/`<`/`>`/`<=`/`>=`/`=` -- all confirmed live to work
             ;; correctly on same-typed bigint args -- are used as-is.
             ;; `quot` is the one exception: cljs's own `quot` internally
             ;; converts to a JS number first and throws on bigint input
             ;; (confirmed live), so division here uses raw `/`, which JS
             ;; BigInt already truncates toward zero (confirmed live for
             ;; both a positive and a negative dividend) -- exactly `quot`'s
             ;; contract.
             (case op
               + (reduce i64-add xs)
               - (if (= 1 (count xs)) (i64-sub 0 (first xs)) (reduce i64-sub xs))
               * (reduce i64-mul xs)
               quot (let [[x y] xs]
                      (when (i64/k-zero? y) (trap! :division-by-zero {}))
                      (when (and (= x i64/min-i64) (= y (js/BigInt -1)))
                        (trap! :signed-division-overflow {}))
                      (/ x y))
               bit-xor (i64/->bigint (apply bit-xor xs))
               bit-and (i64/->bigint (apply bit-and xs))
               bit-or (i64/->bigint (apply bit-or xs))
               = (if (apply = xs) i64/one i64/zero)
               < (if (apply < xs) i64/one i64/zero)
               > (if (apply > xs) i64/one i64/zero)
               <= (if (apply <= xs) i64/one i64/zero)
               >= (if (apply >= xs) i64/one i64/zero))))

        (contains? '#{bit-not i64-shift-left i64-shift-right u64-shift-right} op)
        (let [xs (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (case op
            bit-not (i64-not (first xs))
            i64-shift-left (i64-shl (first xs) (second xs))
            i64-shift-right (i64-shr (first xs) (second xs))
            u64-shift-right (u64-shr (first xs) (second xs))))

        (contains? '#{i32-wrap u32-wrap i32-wrapping-add i32-wrapping-mul i32-xor
                      i32-shift-left i32-shift-right u32-shift-right xorshift32} op)
        (let [xs (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)]
          (case op
            i32-wrap (i32-wrap (first xs))
            u32-wrap (u32-wrap (first xs))
            i32-wrapping-add (i32-add (first xs) (second xs))
            i32-wrapping-mul (i32-mul (first xs) (second xs))
            i32-xor (i32-xor (first xs) (second xs))
            i32-shift-left (i32-shl (first xs) (second xs))
            i32-shift-right (i32-shr (first xs) (second xs))
            u32-shift-right (u32-shr (first xs) (second xs))
            xorshift32 (xorshift32 (first xs))))

        :else
        (let [values (mapv #(eval-expr % env functions fuel heap call-stack cap-call) args)
              ;; Self-tail trampoline for `__kotoba_loop_N` (T7.4): when the
              ;; call target is the current stack tip and that tip is a
              ;; synthesized loop helper, return a trampoline marker instead of
              ;; nesting another JVM frame. Non-helper mutual recursion still
              ;; uses ordinary invoke-function.
              tip (peek call-stack)]
          (if (and (loop-helper-name? op) (= op tip))
            (trampoline-call op values)
            (invoke-function (get functions op) values functions fuel heap call-stack cap-call))))))))

(defn execute
  "Executes one KIR export using normative typed-value semantics. i64 math
  wraps modulo 2^64; bounded strings preserve Unicode text; invalid values,
  division, and resource exhaustion trap.

  `:memory {:base <address> :bytes (volatile! [0..255 ...])}` supplies the
  buffer a byte-walking kernel object reads and writes. It is optional and
  absent by default: with no image every kernel memory operation refuses with
  `:kernel-memory-unavailable`, exactly as before. The `volatile!` is required
  rather than convenient -- the caller reads its stores back out of it, and an
  oracle whose writes are invisible is worth nothing."
  ([kir function-name args] (execute kir function-name args {}))
  ([kir function-name args {:keys [fuel cap-call typed-cap-call pair-capacity kgraph-capacity
                                   memory]
                            :or {fuel default-fuel pair-capacity default-pair-capacity
                                 kgraph-capacity default-kgraph-capacity}}]
   (when (and (contains? kir :exports)
              (not (some #{function-name} (:exports kir))))
     (throw (ex-info "function is not exported" {:phase :ir :function function-name})))
   ;; fuel/pair-capacity/kgraph-capacity are interpreter-internal config,
   ;; never a `.kotoba` value -- plain `integer?` is correct for both
   ;; runtimes here.
   (when-not (and (integer? fuel) (pos? fuel))
     (throw (ex-info "fuel must be a positive integer" {:phase :ir :fuel fuel})))
   (when-not (and (integer? pair-capacity) (<= 0 pair-capacity default-pair-capacity))
     (throw (ex-info "pair capacity is outside runtime limits"
                     {:phase :ir :pair-capacity pair-capacity})))
   (when-not (and (integer? kgraph-capacity) (<= 0 kgraph-capacity default-kgraph-capacity))
     (throw (ex-info "kgraph capacity is outside runtime limits"
                     {:phase :ir :kgraph-capacity kgraph-capacity})))
   (doseq [function (:functions kir)]
     (validated-closure-param-indexes function)
     (validated-i64-pair-chain-param-indexes function)
     (validated-closure-result? function))
   (let [memory-image (validated-memory memory)
         cap-dispatch (when (or cap-call typed-cap-call)
                        (fn
                          ([cap-id value]
                           (if cap-call
                             (cap-call cap-id value)
                             (trap! :capability-denied {:capability cap-id})))
                          ([cap-id request-type result-type request]
                           (if typed-cap-call
                             (typed-cap-call cap-id request-type result-type request)
                             (trap! :capability-denied {:capability cap-id :typed true})))))
         functions (into {} (map (juxt :name identity) (:functions kir)))
         function (get functions function-name)
         param-types (or (:param-types function)
                         (vec (repeat (count (:params function)) :i64)))]
     (when-not (and (sequential? args) (= (count args) (count param-types)))
       (throw (ex-info "arguments do not match function arity" {:phase :ir :args args})))
     (doseq [[arg declared-type] (map vector args param-types)]
       (let [type (if (and (vector? declared-type) (= :ref (first declared-type)))
                    (or (get (:schemas kir) (second declared-type))
                        (throw (ex-info "argument references an unknown schema"
                                        {:phase :ir :schema (second declared-type)})))
                    declared-type)]
       (case type
         :i64 (when-not #?(:clj (and (integer? arg) (<= Long/MIN_VALUE arg Long/MAX_VALUE))
                          :cljs (and (or (i64/bigint-value? arg) (integer? arg))
                                     (i64/in-i64-range? arg)))
                (throw (ex-info "argument must be a signed i64" {:phase :ir :arg arg})))
         :string (value/bounded-string! arg value/string-value-byte-limit)
         :keyword (value/bounded-keyword! arg value/keyword-value-byte-limit)
         :symbol (value/bounded-symbol! arg value/symbol-value-byte-limit)
         :map (value/bounded-map! arg)
         :bool (when-not (boolean? arg)
                 (throw (ex-info "argument must be a boolean" {:phase :ir :arg arg})))
         :option-i64 (value/bounded-option-i64! arg)
         :result-i64 (value/bounded-result-i64! arg)
         :vector-i64 (value/bounded-vector-i64! arg)
         :vector-f64 (value/bounded-vector-f64! arg)
         :string-index (value/bounded-string-index! arg)
         :disjoint-set-i64 (value/bounded-disjoint-set-i64! arg)
         :document (value/bounded-document! arg)
         (value/bounded-typed-value! type arg))))
     (let [invoke #(binding [*runtime-schemas* (:schemas kir)]
                     (invoke-function function
                                    (mapv (fn [arg type]
                                            (if (= type :i64)
                                              (#?(:clj long :cljs i64/->bigint) arg)
                                              arg))
                                          args param-types)
                                    functions
                                    (volatile! fuel) {:cells (volatile! []) :capacity pair-capacity
                                                      :datoms (volatile! []) :kgraph-capacity kgraph-capacity
                                                      :memory memory-image}
                                    [] cap-dispatch))
           ;; Box a `:bool` result at the boundary, the way every other target
           ;; does. `:bool` is a plain 0/1 word inside the interpreter (and
           ;; inside a wasm module, and in the native backends' setcc
           ;; sequences), but the value that LEAVES a target is a host boolean:
           ;; the restricted-ESM emitter returns one, and `kotoba.wasm.core`
           ;; emits an export wrapper that boxes one. The reference was the
           ;; only target still handing back 1/0, so the shared corpora --
           ;; whose whole purpose is that all three agree on the same value --
           ;; could not hold for a predicate.
           ;;
           ;; A `:bool` ARGUMENT has always required a real boolean here (see
           ;; the param check above), so this makes the two directions
           ;; symmetric rather than introducing a new convention.
           box-bool (fn [value]
                      (if (and (= :bool (:result function)) (not (boolean? value)))
                        (not #?(:clj (zero? value) :cljs (i64/k-zero? value)))
                        value))]
       ;; A host with a small native stack can exhaust that stack just before
       ;; the fixed Kotoba call budget does. Host resource errors must never
       ;; escape the language boundary: normalize this one precise failure to
       ;; the same deterministic, fail-closed trap.
       ;;
       ;; BOTH runtimes, and that is the fix rather than the feature. Until
       ;; 2026-08-24 only `:clj` had the guard, so on ClojureScript a raw
       ;; `RangeError: Maximum call stack size exceeded` came out of `lower`
       ;; for a program the JVM rejected with `:fuel-exhausted`. `lower`'s own
       ;; comment about the oracle says unbounded non-helper recursion "still
       ;; traps (fuel or host-stack) and aborts lower -- intentional"; on one
       ;; of the two runtimes it did not.
       ;;
       ;; It stayed hidden because which limit is reached first depends on the
       ;; interpreter, and this repository had no way to notice: every test was
       ;; `.clj` and the fleet gate is `:jvm-test`, so the `:cljs` branch had
       ;; never been executed at all.
       ;;
       ;; How deep the oracle gets before one limit or the other is reached is
       ;; a property of the host AND of the caller's own stack, and it is not
       ;; written down here on purpose: a number in a comment is quoted without
       ;; its date. `kir_host_stack_trap_test` measures it where it stands.
       (try
         (box-bool (invoke))
         ;; `Throwable` on the JVM too, not `StackOverflowError`. Narrowing the
         ;; catch to the one type made the predicate's negative branch
         ;; unreachable there -- nothing else could arrive -- so half of this
         ;; guard could never be shown to work on the runtime that has had it
         ;; the longest. Now both runtimes catch everything the oracle can
         ;; throw and both re-throw what is not host stack exhaustion, which
         ;; `kir_host_stack_trap_test` pins from both directions.
         (catch #?(:clj Throwable :cljs :default) e
           (if (host-stack-exhausted? e)
             (trap! :fuel-exhausted {:limit fuel :host-stack-exhausted true})
             (throw e))))))))

(defn lower [hir]
  (hir/validate! hir)
  (reject-loop-helper-self-calls-off-tail! hir)
  (let [kernel-operations '#{kernel-load-u8 kernel-load-u8-4k kernel-load-u8-16k
                             kernel-store-u8 kernel-store-u8-4k kernel-read-cr2
                             kernel-subregion
                             kernel-load-u32 kernel-store-u32
                             ;; The lock pair marks a module kernel-native for
                             ;; the same reason the MSR pair does: without it
                             ;; the constant oracle would try to evaluate an
                             ;; atomic read-modify-write at compile time. The
                             ;; interpreter above traps rather than answering,
                             ;; so the failure would be loud -- but it would
                             ;; abort the compile of a valid program.
                             kernel-try-lock-u32 kernel-unlock-u32
                             kernel-boot-info kernel-read-cr0 kernel-write-cr0
                             kernel-read-cr3 kernel-write-cr3 kernel-invlpg
                             kernel-read-cs kernel-page-fault-handler-address
                             kernel-rt-timer-handler-address
                             kernel-page-fault-recovery-handler-address
                             kernel-configure-page-fault-recovery kernel-load-idt
                             kernel-double-fault-handler-address
                             kernel-configure-double-fault-ist kernel-load-gdt-tss
                             kernel-probe-guard-write kernel-probe-text-write kernel-probe-nx-execute
                             kernel-probe-recoverable-guard-write kernel-probe-double-fault
                             kernel-cli kernel-sti kernel-hlt kernel-pause
                             kernel-out-u8 kernel-out-u32
                             kernel-in-u8 kernel-in-u32
                             ;; The MSR pair has to be here too, and this is
                             ;; the set that is easy to miss: it is what marks
                             ;; a module kernel-native and thereby suppresses
                             ;; constant-oracling. Without it the oracle would
                             ;; try to evaluate an `rdmsr` at compile time.
                             kernel-read-msr kernel-write-msr
                             ;; And the `cpuid` four, where missing this set
                             ;; would be worse than for `rdmsr`: their operands
                             ;; are LITERALS at every real call site (leaf
                             ;; 0x80000001, subleaf 0), so a constant-folder
                             ;; sees an operation over two constants and has
                             ;; every structural reason to try to evaluate it.
                             ;; The interpreter above traps rather than
                             ;; answering, so the failure would be loud rather
                             ;; than silent -- but the trap would abort the
                             ;; compile of a program that is perfectly valid.
                             ;; A `cpuid` result is a property of the machine,
                             ;; not of the program, and must survive to run
                             ;; time no matter how constant its inputs look.
                             kernel-cpuid-eax kernel-cpuid-ebx
                             kernel-cpuid-ecx kernel-cpuid-edx}
        kernel-native? (some #(and (seq? %) (contains? kernel-operations (first %)))
                             (tree-seq coll? seq (:functions hir)))
        typed-values? (= :kotoba.hir/v3 (:format hir))
        base {:format (if typed-values? :kotoba.kir/v4 :kotoba.kir/v3)
              :entry (:entry hir)
              :exports (:exports hir)
              :schemas (:schemas hir)
              :schema-identities (:schema-identities hir)
              :signature (when (:entry hir) {:params [] :result (:result hir)})
              :effects (:effects hir)
              :functions (mapv (fn [function]
                                 (validated-closure-param-indexes function)
                                 (validated-i64-pair-chain-param-indexes function)
                                 (validated-closure-result? function)
                                 (select-keys function
                                              (cond-> [:name :params :result :effects :body]
                                                typed-values? (conj :param-types)
                                                (contains? function :closure-param-indexes)
                                                (conj :closure-param-indexes)
                                                (contains? function :i64-pair-chain-param-indexes)
                                                (conj :i64-pair-chain-param-indexes)
                                                (contains? function :closure-result?)
                                                (conj :closure-result?))))
                               (:functions hir))}
        ;; Effectful results require host authority and cannot be constant-oracled.
        ;; Deep pure loops (T7.4) need oracle-fuel; loop-helper trampoline keeps
        ;; host stack flat so 10k finishes. Unbounded non-helper recursion still
        ;; traps (fuel or host-stack) and aborts lower — intentional.
        ;; `:bool` folds too. It used to be excluded, which left a pure
        ;; predicate entry with no sealed oracle at all -- and because the
        ;; native path is the only one that seals and re-checks an oracle,
        ;; that alone was what stopped `(defn main [] (< a b))` from
        ;; compiling for native while it compiled for wasm32, js and cljs.
        ;;
        ;; The value sealed here is the BOXED boolean `execute` returns, per
        ;; the boundary convention this namespace already adopted (see
        ;; `box-bool` in `execute`): `:bool` is a 0/1 word inside a module,
        ;; but the value that leaves a target is a host boolean, so wasm's
        ;; export wrapper, the restricted-ESM emitter and this interpreter all
        ;; hand back one. Sealing the boxed value keeps
        ;; `kotoba.verifier`'s own re-execution comparable to it directly.
        value (when (and (:entry hir) (contains? #{:i64 :bool} (:result hir))
                         (empty? (:effects hir)) (not kernel-native?))
                (execute base (:entry hir) [] {:fuel oracle-fuel}))]
    (assoc base
           :oracle-value value
           ;; `:blocks` is the INTERNAL representation and keeps the 0/1 word,
           ;; which is what `:const.i64` can carry and what every target uses
           ;; inside a module. `some?` rather than truthiness, because `false`
           ;; is now a foldable value and must not be mistaken for "no oracle".
           :blocks (if (some? value)
                     [{:id 0 :instructions [[:const.i64 (if (boolean? value)
                                                          (if value 1 0)
                                                          value)]
                                            [:return]]}]
                     []))))
