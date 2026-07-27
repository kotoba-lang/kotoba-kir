(ns kotoba.kir.value
  #?(:clj (:import [java.nio.charset StandardCharsets]
                   [java.security MessageDigest])
     :cljs (:require [kotoba.kir.cljs-i64 :as i64])))

(def string-literal-byte-limit 4096)
(def string-value-byte-limit 65536)
(def keyword-value-byte-limit 512)
(def symbol-value-byte-limit 512)
(def map-entry-limit 128)
(def vector-literal-item-limit 128)
(def vector-item-limit 16384)
;; Canonical ABI `[:list item-descriptor]` item-count bound (see
;; kotoba.wasm.canonical-abi's `list-layout`). Deliberately the same
;; order of magnitude as this file's other bounded-sequential-collection
;; limits (`vector-item-limit` above) rather than a freshly invented number;
;; kept as its own named constant rather than a direct reuse of
;; `vector-item-limit` because the two bound genuinely different domains
;; (a `vector-i64` runtime value vs. a Canonical ABI wire-transport schema
;; shape whose item type is not restricted to i64) that happen to share a
;; magnitude today, not one concept wearing two names.
(def canonical-list-item-limit 16384)
;; Shared across every nested `[:list ...]` node in one value graph. Keeping
;; this equal to the flat per-list ceiling preserves existing flat-list
;; capacity while preventing list-of-list cardinalities from multiplying.
(def canonical-list-total-item-limit 16384)
;; A collection of individually valid strings must not multiply the per-leaf
;; bound into an unbounded host allocation. All string/keyword leaves in one
;; canonical typed value share this aggregate UTF-8 payload budget.
(def canonical-indirect-byte-limit 1048576)
(def adt-depth-limit 8)
(def adt-node-limit 64)
(def variant-case-limit 32)
(def heterogeneous-vector-item-limit 32)
(def typed-set-item-limit 32)
(def typed-map-entry-limit 31)
(def record-field-limit 32)
;; Canonical ABI `[:tuple item-descriptor ...]` item-count bound (see
;; kotoba.wasm.canonical-abi's `tuple-layout`). A tuple is a structural,
;; positional analog of a sealed record's fields, so this is kept at the same
;; magnitude as this file's own nominal `record-field-limit` above (a sealed
;; record's own field-count bound) and `heterogeneous-vector-item-limit`
;; above (this file's pre-existing fixed-length heterogeneous-product bound,
;; which `kotoba.component.wit` already renders to this exact WIT
;; `tuple<...>` syntax today) -- both already 32. Kept as its own named
;; constant rather than a direct reuse of either, for the same reason
;; `canonical-list-item-limit` above got its own name alongside
;; `vector-item-limit`: these bound genuinely different domains that happen
;; to share a magnitude today, not one concept wearing three names.
(def canonical-tuple-item-limit 32)
(def compact-graph-item-limit 128)
(def string-index-key-byte-limit 65536)
(def document-depth-limit 8)
(def document-node-limit 256)
(def document-container-item-limit 32)
(def document-utf8-byte-limit 65536)

(defn f64-value? [value]
  #?(:clj (instance? Double value)
     :cljs (number? value)))

(defn f32-value? [value]
  #?(:clj (instance? Float value)
     :cljs (and (number? value) (js/Object.is (js/Math.fround value) value))))

(defn f32-to-i64-bits [value]
  (when-not (f32-value? value)
    (throw (ex-info "value is not f32" {:phase :value :value value})))
  #?(:clj (long (Float/floatToIntBits ^float value))
     :cljs (if (js/Number.isNaN value)
             (js/BigInt 2143289344)
             (let [buffer (js/ArrayBuffer. 4)
                   view (js/DataView. buffer)]
               (.setFloat32 view 0 value true)
               (js/BigInt (.getInt32 view 0 true))))))

(defn i64-bits-to-f32 [bits]
  #?(:clj (do
            (when-not (and (integer? bits) (<= Integer/MIN_VALUE bits Integer/MAX_VALUE))
              (throw (ex-info "f32 bit pattern is not signed i32" {:phase :value :value bits})))
            (Float/intBitsToFloat (int bits)))
     :cljs (let [min (js/BigInt -2147483648) max (js/BigInt 2147483647)]
             (when-not (and (i64/bigint-value? bits) (<= min bits max))
               (throw (ex-info "f32 bit pattern is not signed i32" {:phase :value :value bits})))
             (let [buffer (js/ArrayBuffer. 4)
                   view (js/DataView. buffer)]
               (.setInt32 view 0 (js/Number bits) true)
               (.getFloat32 view 0 true)))))

(defn f64-to-f32-rounded [value]
  (when-not (f64-value? value)
    (throw (ex-info "value is not f64" {:phase :value :value value})))
  #?(:clj (.floatValue ^Number value) :cljs (js/Math.fround value)))

(defn f32-to-f64-exact [value]
  (when-not (f32-value? value)
    (throw (ex-info "value is not f32" {:phase :value :value value})))
  #?(:clj (double value) :cljs value))

(defn i64-to-f32-rounded [value]
  #?(:clj (do
            (when-not (and (integer? value) (<= Long/MIN_VALUE value Long/MAX_VALUE))
              (throw (ex-info "value is not a signed i64" {:phase :value})))
            (.floatValue ^Number value))
     :cljs (do
             (when-not (and (i64/bigint-value? value) (i64/in-i64-range? value))
               (throw (ex-info "value is not a signed i64" {:phase :value})))
             (js/Math.fround (js/Number value)))))

(defn i64-to-f32-checked [value]
  (let [result (i64-to-f32-rounded value)
        exact? #?(:clj (= (bigint value)
                          (bigint (.toBigIntegerExact
                                   (java.math.BigDecimal/valueOf (double result)))))
                  :cljs (= value (js/BigInt result)))]
    (when-not exact?
      (throw (ex-info "i64 is not exactly representable as f32"
                      {:phase :value :conversion :i64-to-f32-checked})))
    result))

(defn f64-to-i64-bits [value]
  (when-not (f64-value? value)
    (throw (ex-info "value is not f64" {:phase :value :value value})))
  #?(:clj (Double/doubleToLongBits ^double value)
     :cljs (let [buffer (js/ArrayBuffer. 8)
                 view (js/DataView. buffer)]
             (.setFloat64 view 0 value true)
             (.getBigInt64 view 0 true))))

(defn i64-bits-to-f64 [bits]
  #?(:clj (do
            (when-not (and (integer? bits) (<= Long/MIN_VALUE bits Long/MAX_VALUE))
              (throw (ex-info "f64 bit pattern is not i64" {:phase :value :value bits})))
            (Double/longBitsToDouble (long bits)))
     :cljs (let [buffer (js/ArrayBuffer. 8)
                 view (js/DataView. buffer)]
             (when-not (and (i64/bigint-value? bits) (i64/in-i64-range? bits))
               (throw (ex-info "f64 bit pattern is not i64" {:phase :value :value bits})))
             (.setBigInt64 view 0 bits true)
             (.getFloat64 view 0 true))))

(defn i64-to-f64-rounded [value]
  #?(:clj (do
            (when-not (and (integer? value) (<= Long/MIN_VALUE value Long/MAX_VALUE))
              (throw (ex-info "value is not a signed i64" {:phase :value})))
            (double value))
     :cljs (do
             (when-not (and (i64/bigint-value? value) (i64/in-i64-range? value))
               (throw (ex-info "value is not a signed i64" {:phase :value})))
             (js/Number value))))

(defn i64-to-f64-checked [value]
  (let [result (i64-to-f64-rounded value)
        exact? #?(:clj (= (bigint value)
                          (bigint (.toBigIntegerExact (java.math.BigDecimal/valueOf result))))
                  :cljs (= value (js/BigInt result)))]
    (when-not exact?
      (throw (ex-info "i64 is not exactly representable as f64"
                      {:phase :value :conversion :i64-to-f64-checked})))
    result))

(defn- checked-i64-result [value conversion]
  (when-not #?(:clj (<= (bigint Long/MIN_VALUE) value (bigint Long/MAX_VALUE))
               :cljs (i64/in-i64-range? value))
    (throw (ex-info "f64 conversion is outside signed i64 range"
                    {:phase :value :conversion conversion})))
  #?(:clj (long value) :cljs value))

(defn f64-to-i64-checked [value]
  (when-not (and (f64-value? value)
                 #?(:clj (Double/isFinite ^double value) :cljs (js/Number.isFinite value))
                 #?(:clj (= value (Math/rint ^double value)) :cljs (js/Number.isInteger value)))
    (throw (ex-info "f64 is not a finite integral value"
                    {:phase :value :conversion :f64-to-i64-checked})))
  (checked-i64-result
   #?(:clj (bigint (.toBigIntegerExact (java.math.BigDecimal/valueOf value)))
      :cljs (js/BigInt value))
   :f64-to-i64-checked))

(defn f64-to-i64-truncating [value]
  (when-not (and (f64-value? value)
                 #?(:clj (Double/isFinite ^double value) :cljs (js/Number.isFinite value)))
    (throw (ex-info "f64 is not finite"
                    {:phase :value :conversion :f64-to-i64-truncating})))
  (checked-i64-result
   #?(:clj (bigint (.toBigInteger (java.math.BigDecimal/valueOf value)))
      :cljs (js/BigInt (js/Math.trunc value)))
   :f64-to-i64-truncating))

(defn f32-to-i64-checked [value]
  (when-not (and (f32-value? value)
                 #?(:clj (Float/isFinite ^float value) :cljs (js/Number.isFinite value))
                 #?(:clj (= value (Math/rint (double value))) :cljs (js/Number.isInteger value)))
    (throw (ex-info "f32 is not a finite integral value"
                    {:phase :value :conversion :f32-to-i64-checked})))
  (checked-i64-result
   #?(:clj (bigint (.toBigIntegerExact (java.math.BigDecimal/valueOf (double value))))
      :cljs (js/BigInt value))
   :f32-to-i64-checked))

(defn f32-to-i64-truncating [value]
  (when-not (and (f32-value? value)
                 #?(:clj (Float/isFinite ^float value) :cljs (js/Number.isFinite value)))
    (throw (ex-info "f32 is not finite"
                    {:phase :value :conversion :f32-to-i64-truncating})))
  (checked-i64-result
   #?(:clj (bigint (.toBigInteger (java.math.BigDecimal/valueOf (double value))))
      :cljs (js/BigInt (js/Math.trunc value)))
   :f32-to-i64-truncating))

(defn utf8-byte-count!
  "Return the exact UTF-8 byte count without normalizing or replacing malformed
  UTF-16. Unpaired surrogates fail closed on both JVM and JavaScript hosts."
  [value]
  (when-not (string? value)
    (throw (ex-info "value is not a string" {:phase :value :value value})))
  (loop [index 0 total 0]
    (if (= index (count value))
      total
      (let [unit #?(:clj (int (.charAt ^String value index))
                    :cljs (.charCodeAt value index))]
        (cond
          (<= unit 0x7f) (recur (inc index) (inc total))
          (<= unit 0x7ff) (recur (inc index) (+ total 2))
          (<= 0xd800 unit 0xdbff)
          (if (< (inc index) (count value))
            (let [next-unit #?(:clj (int (.charAt ^String value (inc index)))
                               :cljs (.charCodeAt value (inc index)))]
              (if (<= 0xdc00 next-unit 0xdfff)
                (recur (+ index 2) (+ total 4))
                (throw (ex-info "string contains an unpaired high surrogate"
                                {:phase :value :index index}))))
            (throw (ex-info "string contains an unpaired high surrogate"
                            {:phase :value :index index})))
          (<= 0xdc00 unit 0xdfff)
          (throw (ex-info "string contains an unpaired low surrogate"
                          {:phase :value :index index}))
          :else (recur (inc index) (+ total 3)))))))

(defn bounded-string!
  [value limit]
  (let [bytes (utf8-byte-count! value)]
    (when (> bytes limit)
      (throw (ex-info "string exceeds UTF-8 byte limit"
                      {:phase :value :bytes bytes :limit limit})))
    value))

(defn utf8-substring!
  "Checked UTF-8 byte-offset substring. Both offsets must be code-point
  boundaries; malformed UTF-16 is rejected by utf8-byte-count! first."
  [value start end]
  (let [length (utf8-byte-count! value)]
    (when-not (and (integer? start) (integer? end) (<= 0 start end length))
      (throw (ex-info "string substring indexes are out of bounds"
                      {:phase :value :start start :end end :length length})))
    (loop [index 0 byte-index 0 boundaries {0 0}]
      (if (= index (count value))
        (let [from (get boundaries start) to (get boundaries end)]
          (when-not (and (some? from) (some? to))
            (throw (ex-info "string substring index splits a UTF-8 code point"
                            {:phase :value :start start :end end})))
          (subs value from to))
        (let [unit #?(:clj (int (.charAt ^String value index))
                      :cljs (.charCodeAt value index))
              [units bytes] (cond
                              (<= unit 0x7f) [1 1]
                              (<= unit 0x7ff) [1 2]
                              (<= 0xd800 unit 0xdbff) [2 4]
                              :else [1 3])]
          (recur (+ index units) (+ byte-index bytes)
                 (assoc boundaries (+ byte-index bytes) (+ index units))))))))

(defn utf8-code-point-at!
  "Return the Unicode code point of the UTF-8 sequence that STARTS at BYTE-OFFSET
  (a UTF-8 byte offset into VALUE, same coordinate space as utf8-substring!'s
  offsets and string-byte-length). BYTE-OFFSET must be a code-point boundary in
  [0, byte-length); anything else traps. The guest can derive the code point's
  UTF-8 byte width from the returned value (< 0x80 -> 1, < 0x800 -> 2,
  < 0x10000 -> 3, else 4) to advance, so a single op is enough to walk a string."
  [value byte-offset]
  (let [length (utf8-byte-count! value)]
    (when-not (and (integer? byte-offset) (<= 0 byte-offset) (< byte-offset length))
      (throw (ex-info "string code-point offset is out of bounds"
                      {:phase :value :offset byte-offset :length length})))
    (loop [index 0 byte-index 0]
      (when (= index (count value))
        (throw (ex-info "string code-point offset splits a UTF-8 code point"
                        {:phase :value :offset byte-offset})))
      (let [unit #?(:clj (int (.charAt ^String value index))
                    :cljs (.charCodeAt value index))
            [units bytes code-point]
            (cond
              (<= unit 0x7f) [1 1 unit]
              (<= unit 0x7ff) [1 2 unit]
              (<= 0xd800 unit 0xdbff)
              (let [next-unit #?(:clj (int (.charAt ^String value (inc index)))
                                 :cljs (.charCodeAt value (inc index)))]
                [2 4 (+ 0x10000
                        (bit-shift-left (- unit 0xd800) 10)
                        (- next-unit 0xdc00))])
              :else [1 3 unit])]
        (cond
          (= byte-index byte-offset) code-point
          (> (+ byte-index bytes) byte-offset)
          (throw (ex-info "string code-point offset splits a UTF-8 code point"
                          {:phase :value :offset byte-offset}))
          :else (recur (+ index units) (+ byte-index bytes)))))))

(defn bounded-keyword!
  [value limit]
  (when-not (keyword? value)
    (throw (ex-info "value is not a keyword" {:phase :value :value value})))
  (let [text (str value)
        bytes (utf8-byte-count! text)]
    (when (> bytes limit)
      (throw (ex-info "keyword exceeds UTF-8 byte limit"
                      {:phase :value :bytes bytes :limit limit})))
    value))

(defn bounded-symbol!
  [value limit]
  (when-not (symbol? value)
    (throw (ex-info "value is not a symbol" {:phase :value :value value})))
  (let [text (str value)
        bytes (utf8-byte-count! text)]
    (when (> bytes limit)
      (throw (ex-info "symbol exceeds UTF-8 byte limit"
                      {:phase :value :bytes bytes :limit limit})))
    value))

;; JVM `clojure.string/lower-case` (and bare `.toLowerCase()`) fold through
;; the platform DEFAULT locale, which is not deterministic across hosts --
;; the classic case is Turkish (`tr`/`tr-TR`), where uppercase `I` folds to
;; dotless `ı`, not `i`. A safe deterministic application language cannot
;; let case-folding depend on which machine it runs on, so this always pins
;; `Locale/ROOT` on the JVM. cljs's `.toLowerCase()` (no-arg) is already
;; locale-independent Unicode simple case mapping and is the closest cljs
;; equivalent; the two are verified to agree only on the ASCII and common
;; accented-Latin ranges this primitive's conformance vectors cover, not
;; claimed to agree on the full Unicode SpecialCasing table (Turkish `İ`/`ı`,
;; German `ß`, Lithuanian dot-retention, and similar locale/context-sensitive
;; exceptions are explicitly out of scope).
(defn fold-case!
  [value]
  (when-not (string? value)
    (throw (ex-info "value is not a string" {:phase :value :value value})))
  #?(:clj (.toLowerCase ^String value java.util.Locale/ROOT)
     :cljs (.toLowerCase value)))

(defn bounded-map!
  "Validate the first bounded map profile: canonical keyword keys and i64
  values only. The representation is immutable host data, never a pointer or
  integer sentinel in the Kotoba value domain."
  [value]
  (when-not (map? value)
    (throw (ex-info "value is not a map" {:phase :value :value value})))
  (when (> (count value) map-entry-limit)
    (throw (ex-info "map exceeds entry limit"
                    {:phase :value :entries (count value) :limit map-entry-limit})))
  (doseq [[key item] value]
    (bounded-keyword! key keyword-value-byte-limit)
    (when-not #?(:clj (and (integer? item)
                            (<= Long/MIN_VALUE item Long/MAX_VALUE))
                 :cljs (and (i64/bigint-value? item)
                            (i64/in-i64-range? item)))
      (throw (ex-info "map value is not a signed i64"
                      {:phase :value :key key}))))
  value)

(defn bounded-option-i64!
  "Validate the first option profile. None is `[false]`; some i64 is
  `[true value]`. Nil, host null/undefined, and integer sentinels are never
  members of the runtime value domain. Return a canonical immutable value."
  [value]
  (when-not (and (vector? value)
                 (or (= [false] value)
                     (and (= 2 (count value)) (true? (first value)))))
    (throw (ex-info "value is not a tagged option-i64"
                    {:phase :value :value value})))
  (if (false? (first value))
    [false]
    (let [item (second value)]
      (when-not #?(:clj (and (integer? item)
                              (<= Long/MIN_VALUE item Long/MAX_VALUE))
                   :cljs (and (i64/bigint-value? item)
                              (i64/in-i64-range? item)))
        (throw (ex-info "option payload is not a signed i64" {:phase :value})))
      [true item])))

(defn bounded-result-i64!
  "Validate the first algebraic-result profile. `[true value]` is ok and
  `[false error]` is err; both variants carry exactly one signed-i64 payload."
  [value]
  (when-not (and (vector? value) (= 2 (count value)) (boolean? (first value)))
    (throw (ex-info "value is not a tagged result-i64"
                    {:phase :value :value value})))
  (let [item (second value)]
    (when-not #?(:clj (and (integer? item) (<= Long/MIN_VALUE item Long/MAX_VALUE))
                 :cljs (and (i64/bigint-value? item) (i64/in-i64-range? item)))
      (throw (ex-info "result payload is not a signed i64" {:phase :value})))
    [(first value) item]))

(defn bounded-vector-i64!
  "Validate and return the first bounded sequential collection profile."
  [value]
  (when-not (vector? value)
    (throw (ex-info "value is not a vector-i64" {:phase :value :value value})))
  (when (> (count value) vector-item-limit)
    (throw (ex-info "vector exceeds item limit"
                    {:phase :value :items (count value) :limit vector-item-limit})))
  (doseq [item value]
    (when-not #?(:clj (and (integer? item) (<= Long/MIN_VALUE item Long/MAX_VALUE))
                 :cljs (and (i64/bigint-value? item) (i64/in-i64-range? item)))
      (throw (ex-info "vector item is not a signed i64" {:phase :value}))))
  value)

(defn bounded-vector-f64!
  "Validate a bounded homogeneous IEEE-754 binary64 vector."
  [value]
  (when-not (vector? value)
    (throw (ex-info "value is not a vector-f64" {:phase :value :value value})))
  (when (> (count value) vector-item-limit)
    (throw (ex-info "vector-f64 exceeds item limit"
                    {:phase :value :items (count value) :limit vector-item-limit})))
  (doseq [item value]
    (when-not (f64-value? item)
      (throw (ex-info "vector-f64 item is not f64" {:phase :value}))))
  value)

(declare bounded-typed-value!)

(defn bounded-string-index!
  "Validate the compact, canonical string -> i64 graph index. The aggregate
  UTF-8 key budget is shared by the whole value and keys must be strictly
  ordered, making duplicate admission impossible."
  [value]
  (when-not (and (vector? value)
                 (<= (count value) compact-graph-item-limit)
                 (every? #(and (vector? %) (= 2 (count %))) value))
    (throw (ex-info "value is not a compact string index"
                    {:phase :value :limit compact-graph-item-limit})))
  (let [entries (mapv (fn [[key item]]
                        [(bounded-string! key string-value-byte-limit)
                         (bounded-typed-value! :i64 item)])
                      value)
        total-bytes (reduce + (map (comp utf8-byte-count! first) entries))]
    (when (> total-bytes string-index-key-byte-limit)
      (throw (ex-info "string index exceeds aggregate UTF-8 key budget"
                      {:phase :value :bytes total-bytes
                       :limit string-index-key-byte-limit})))
    (when-not (every? (fn [[[left _] [right _]]] (neg? (compare left right)))
                      (partition 2 1 entries))
      (throw (ex-info "string index keys are duplicated or non-canonical"
                      {:phase :value})))
    entries))

(defn bounded-disjoint-set-i64!
  "Validate a canonical persistent union-find value. Parents always point
  inside the set; ranks are non-negative and bounded by the item count."
  [value]
  (when-not (and (vector? value) (= 2 (count value))
                 (vector? (first value)) (vector? (second value)))
    (throw (ex-info "value is not a compact disjoint set" {:phase :value})))
  (let [[parents ranks] value
        size (count parents)]
    (when-not (and (<= size compact-graph-item-limit) (= size (count ranks)))
      (throw (ex-info "disjoint set exceeds item limit or has mismatched arrays"
                      {:phase :value :limit compact-graph-item-limit})))
    (doseq [parent parents]
      (bounded-typed-value! :i64 parent)
      (when-not (< -1 parent size)
        (throw (ex-info "disjoint set parent is out of range" {:phase :value}))))
    (doseq [rank ranks]
      (bounded-typed-value! :i64 rank)
      (when-not (<= 0 rank size)
        (throw (ex-info "disjoint set rank is out of range" {:phase :value}))))
    (doseq [start (range size)]
      (loop [current start remaining (inc size)]
        (when (zero? remaining)
          (throw (ex-info "disjoint set parent graph contains a cycle" {:phase :value})))
        (let [parent #?(:clj (int (nth parents current))
                        :cljs (js/Number (nth parents current)))]
          (when-not (= parent current)
            (recur parent (dec remaining))))))
    [parents ranks]))

(defn bounded-document!
  "Validate and rebuild the canonical tagged document tree. Document maps use
  keyword keys sorted by their full textual representation; document values
  never contain host objects or ambient references."
  [value]
  (let [nodes (volatile! 0)
        bytes (volatile! 0)
        charge-text! (fn [text]
                       (vswap! bytes + (utf8-byte-count! text))
                       (when (> @bytes document-utf8-byte-limit)
                         (throw (ex-info "document exceeds aggregate UTF-8 limit"
                                         {:phase :value :limit document-utf8-byte-limit}))))]
    (letfn [(walk [node depth]
              (when (> depth document-depth-limit)
                (throw (ex-info "document exceeds depth limit"
                                {:phase :value :limit document-depth-limit})))
              (vswap! nodes inc)
              (when (> @nodes document-node-limit)
                (throw (ex-info "document exceeds node limit"
                                {:phase :value :limit document-node-limit})))
              (when-not (and (vector? node) (string? (first node)))
                (throw (ex-info "value is not a tagged document node" {:phase :value})))
              (let [[tag payload & extra] node]
                (when (seq extra)
                  (throw (ex-info "document node has excess fields" {:phase :value :tag tag})))
                (case tag
                  "null" (do (when-not (= 1 (count node))
                               (throw (ex-info "invalid document null" {:phase :value})))
                             ["null"])
                  "bool" [tag (do (when-not (boolean? payload)
                                     (throw (ex-info "invalid document bool" {:phase :value})))
                                   payload)]
                  "i64" [tag (do (when-not #?(:clj (and (integer? payload)
                                                          (<= Long/MIN_VALUE payload Long/MAX_VALUE))
                                               :cljs (and (i64/bigint-value? payload)
                                                          (i64/in-i64-range? payload)))
                                    (throw (ex-info "invalid document i64" {:phase :value})))
                                  payload)]
                  "f64" [tag (do (when-not (and (f64-value? payload)
                                                 #?(:clj (Double/isFinite ^double payload)
                                                    :cljs (js/Number.isFinite payload)))
                                    (throw (ex-info "invalid document f64" {:phase :value})))
                                  payload)]
                  "string" [tag (do (bounded-string! payload string-value-byte-limit)
                                     (charge-text! payload) payload)]
                  "keyword" [tag (do (bounded-keyword! payload keyword-value-byte-limit)
                                      (charge-text! (str payload)) payload)]
                  "vector"
                  (do (when-not (and (vector? payload)
                                     (<= (count payload) document-container-item-limit))
                        (throw (ex-info "invalid document vector"
                                        {:phase :value :limit document-container-item-limit})))
                      [tag (mapv #(walk % (inc depth)) payload)])
                  "map"
                  (do (when-not (and (vector? payload)
                                     (<= (count payload) document-container-item-limit)
                                     (every? #(and (vector? %) (= 2 (count %))) payload))
                        (throw (ex-info "invalid document map"
                                        {:phase :value :limit document-container-item-limit})))
                      (let [keys (mapv first payload)
                            _ (doseq [key keys]
                                (bounded-keyword! key keyword-value-byte-limit)
                                (charge-text! (str key)))
                            canonical (vec (sort-by (comp str first) payload))]
                        (when-not (and (= payload canonical)
                                       (= (count keys) (count (distinct keys))))
                          (throw (ex-info "document map keys are duplicate or noncanonical"
                                          {:phase :value})))
                        [tag (mapv (fn [[key item]] [key (walk item (inc depth))]) payload)]))
                  (throw (ex-info "unknown document tag" {:phase :value :tag tag})))))]
      (walk value 0))))

(defn- utf8-bytes
  "UTF-8 byte sequence for S as a seq of 0-255 ints."
  [s]
  #?(:clj (map #(bit-and (int %) 0xff) (.getBytes ^String s StandardCharsets/UTF_8))
     :cljs (js->clj (.encode (js/TextEncoder.) s))))

(defn- normalize-document-f64 [value]
  ;; Match document-equal? identity for signed zero.
  (if (zero? value) 0.0 value))

(defn document-canonical-bytes
  "Deterministic UTF-8 identity encoding of a validated document. Format:
  n | b t/f | i <decimal> ; | f <i64-bits-decimal> ; |
  s <utf8-len> : <bytes> | k <utf8-len> : <keyword-str-with-colon-bytes> |
  v <count> : <items...> | m <count> : (K <key-len> : <key-bytes> <item>)*

  Keywords (values and map keys) use the full `str` form including the leading
  colon (e.g. \":tag\"). Map keys are tagged with capital K to distinguish them
  from keyword values (lowercase k). Map order is the already-canonical sorted
  order from bounded-document!."
  [value]
  (let [doc (bounded-document! value)
        out #?(:clj (java.util.ArrayList.)
               :cljs (array))]
    (letfn [(emit [n]
              #?(:clj (.add out (int n))
                 :cljs (.push out n)))
            (emit-str [s]
              (doseq [b (utf8-bytes s)] (emit b)))
            (emit-len-str [s]
              (let [bs (utf8-bytes s)]
                (emit-str (str (count bs)))
                (emit (int \:))
                (doseq [b bs] (emit b))))
            (walk [node]
              (let [tag (first node)]
                (case tag
                  "null" (emit (int \n))
                  "bool" (do (emit (int \b))
                             (emit (if (second node) (int \t) (int \f))))
                  "i64" (do (emit (int \i))
                            (emit-str (str (second node)))
                            (emit (int \;)))
                  "f64" (do (emit (int \f))
                            (emit-str (str (f64-to-i64-bits (normalize-document-f64 (second node)))))
                            (emit (int \;)))
                  "string" (do (emit (int \s))
                               (emit-len-str (second node)))
                  "keyword" (do (emit (int \k))
                                (emit-len-str (str (second node))))
                  "vector" (do (emit (int \v))
                               (emit-str (str (count (second node))))
                               (emit (int \:))
                               (doseq [item (second node)] (walk item)))
                  "map" (do (emit (int \m))
                            (emit-str (str (count (second node))))
                            (emit (int \:))
                            (doseq [[k item] (second node)]
                              (emit (int \K))
                              (emit-len-str (str k))
                              (walk item)))
                  (throw (ex-info "unknown document tag in canonical encoding"
                                  {:phase :value :tag tag})))))]
      (walk doc)
      #?(:clj (let [arr (byte-array (.size out))]
                (dotimes [i (.size out)]
                  (aset-byte arr i (unchecked-byte (.get out i))))
                arr)
         :cljs (js/Uint8Array.from out)))))

(defn document-sha256-hex
  "SHA-256 hex digest of document-canonical-bytes. Host-independent identity
  for logical documents (W4 exit gate)."
  [value]
  (let [bytes (document-canonical-bytes value)]
    #?(:clj (let [digest (.digest (MessageDigest/getInstance "SHA-256") ^bytes bytes)]
              (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest)))
       :cljs (throw (js/Error. "document-sha256-hex requires the JVM/Node host path")))))

(def ^:private leaf-value-types
  #{:i64 :f32 :f64 :string :keyword :symbol :map :bool :option-i64 :result-i64
    :vector-i64 :vector-f64 :string-index :disjoint-set-i64 :document})

(defn validate-value-type!
  ([type] (validate-value-type! type 0 (volatile! 0)))
  ([type depth nodes]
   (vswap! nodes inc)
   (when (> @nodes adt-node-limit)
     (throw (ex-info "value type exceeds node limit" {:phase :value :limit adt-node-limit})))
   (when (> depth adt-depth-limit)
     (throw (ex-info "value type exceeds depth limit" {:phase :value :limit adt-depth-limit})))
   (cond
     (contains? leaf-value-types type)
     type
     (and (vector? type) (= 2 (count type)) (= :ref (first type))
          (keyword? (second type)) (namespace (second type)))
     type
     (and (vector? type) (= 3 (count type)) (= :result (first type)))
     (do (validate-value-type! (second type) (inc depth) nodes)
         (validate-value-type! (nth type 2) (inc depth) nodes)
         type)
     (and (vector? type) (= 2 (count type)) (= :option (first type)))
     (do (validate-value-type! (second type) (inc depth) nodes)
         type)
     (and (vector? type) (= 2 (count type)) (= :list (first type)))
     (do (validate-value-type! (second type) (inc depth) nodes)
         type)
     (and (vector? type) (= 2 (count type)) (= :vector (first type)))
     (let [item-types (second type)]
       (when-not (and (vector? item-types)
                      (<= (count item-types) heterogeneous-vector-item-limit))
         (throw (ex-info "heterogeneous vector types are invalid"
                         {:phase :value :limit heterogeneous-vector-item-limit})))
       (vswap! nodes inc)
       (when (> @nodes adt-node-limit)
         (throw (ex-info "value type exceeds node limit"
                         {:phase :value :limit adt-node-limit})))
       (doseq [item-type item-types]
         (validate-value-type! item-type (inc depth) nodes))
       type)
     (and (vector? type) (= 2 (count type)) (= :set (first type)))
     (do (when (contains? #{:f32 :f64} (second type))
           (throw (ex-info "direct floating set items are outside the structured scalar ABI"
                           {:phase :value :type type})))
         (validate-value-type! (second type) (inc depth) nodes)
         type)
     (and (vector? type) (= 3 (count type)) (= :map (first type)))
     (do (when (some #{:f32 :f64} [(second type) (nth type 2)])
           (throw (ex-info "direct floating map keys or values are outside the structured scalar ABI"
                           {:phase :value :type type})))
         (validate-value-type! (second type) (inc depth) nodes)
         (validate-value-type! (nth type 2) (inc depth) nodes)
         type)
     (and (vector? type) (= 3 (count type)) (= :record (first type)))
     (let [[_ type-id fields] type]
       (when-not (and (keyword? type-id) (namespace type-id))
         (throw (ex-info "record type id must be a qualified keyword" {:phase :value})))
       (when-not (and (vector? fields) (seq fields) (<= (count fields) record-field-limit)
                      (every? #(and (vector? %) (= 2 (count %)) (keyword? (first %))) fields)
                      (= (count fields) (count (distinct (map first fields)))))
         (throw (ex-info "record fields are invalid" {:phase :value})))
       (vswap! nodes + (+ 2 (* 2 (count fields))))
       (when (> @nodes adt-node-limit)
         (throw (ex-info "value type exceeds node limit" {:phase :value :limit adt-node-limit})))
       (doseq [[_ field-type] fields]
         (validate-value-type! field-type (inc depth) nodes))
       type)
     (and (vector? type) (= 3 (count type)) (= :variant (first type)))
     (let [[_ type-id cases] type]
       (when-not (and (keyword? type-id) (namespace type-id))
         (throw (ex-info "variant type id must be a qualified keyword" {:phase :value})))
       (when-not (and (vector? cases) (seq cases) (<= (count cases) variant-case-limit)
                      (every? #(and (vector? %) (= 2 (count %)) (keyword? (first %))) cases)
                      (= (count cases) (count (distinct (map first cases)))))
         (throw (ex-info "variant cases are invalid" {:phase :value})))
       (vswap! nodes + (+ 2 (* 2 (count cases))))
       (when (> @nodes adt-node-limit)
         (throw (ex-info "value type exceeds node limit" {:phase :value :limit adt-node-limit})))
       (doseq [[_ payload-type] cases]
         (validate-value-type! payload-type (inc depth) nodes))
       type)
     :else (throw (ex-info "value type is outside the safe profile"
                           {:phase :value :type type})))))

(declare compare-typed-values)

(defn- compare-sequences [types left right]
  (loop [remaining-types (seq types) left-items (seq left) right-items (seq right)]
    (cond
      (and (nil? left-items) (nil? right-items)) 0
      (nil? left-items) -1
      (nil? right-items) 1
      :else (let [comparison (compare-typed-values (first remaining-types)
                                                   (first left-items) (first right-items))]
              (if (zero? comparison)
                (recur (next remaining-types) (next left-items) (next right-items))
                comparison)))))

(defn- schema-ref-type?
  [type]
  (and (vector? type) (= 2 (count type)) (= :ref (first type))
       (keyword? (second type)) (namespace (second type))))

(defn- nominal-value-type
  "Full :variant / :record descriptor carried as slot 0 of a nominal value."
  [value]
  (when (vector? value)
    (let [head (first value)]
      (when (and (vector? head) (#{:variant :record} (first head))
                 (keyword? (second head)) (namespace (second head)))
        head))))

(defn- resolve-schema-ref-type
  "Resolve [:ref :ns/name] against a value that carries its full nominal type.
  Handles stay out of the application model: the value is still the full
  tagged tree; the ref is only a type-level alias into a sealed schema."
  [type value]
  (let [root (second type)
        value-type (nominal-value-type value)]
    (when-not (and value-type (= root (second value-type)))
      (throw (ex-info "value is not the declared schema ref"
                      {:phase :value :ref root :value-type value-type})))
    value-type))

(defn compare-typed-values
  "Language-owned total order for already validated values of one type."
  [type left right]
  (case type
    :i64 (compare left right)
    :string (compare left right)
    :keyword (compare (str left) (str right))
    :symbol (compare (str left) (str right))
    :bool (compare left right)
    :option-i64 (if (= (first left) (first right))
                  (if (first left) (compare (second left) (second right)) 0)
                  (if (first left) 1 -1))
    :result-i64 (if (= (first left) (first right))
                  (compare (second left) (second right))
                  (if (first left) 1 -1))
     :vector-i64 (compare-sequences (repeat (max (count left) (count right)) :i64)
                                   left right)
    :string-index (compare-sequences
                   (cycle [:string :i64]) (mapcat identity left) (mapcat identity right))
    :disjoint-set-i64 (let [parents-comparison
                            (compare-sequences (repeat (max (count (first left))
                                                            (count (first right))) :i64)
                                               (first left) (first right))]
                        (if (zero? parents-comparison)
                          (compare-sequences (repeat (max (count (second left))
                                                          (count (second right))) :i64)
                                             (second left) (second right))
                          parents-comparison))
    :map (let [left-items (mapcat identity left)
               right-items (mapcat identity right)
               types (cycle [:keyword :i64])]
           (compare-sequences types left-items right-items))
    (cond
      (schema-ref-type? type)
      (let [left-type (resolve-schema-ref-type type left)
            right-type (resolve-schema-ref-type type right)]
        (when-not (= left-type right-type)
          (throw (ex-info "schema ref values disagree on nominal type"
                          {:phase :value :left left-type :right right-type})))
        (compare-typed-values left-type left right))

      (= :option (first type))
      (if (= (second left) (second right))
        (if (second left)
          (compare-typed-values (second type) (nth left 2) (nth right 2)) 0)
        (if (second left) 1 -1))

      (= :result (first type))
      (if (= (first left) (first right))
        (compare-typed-values (if (first left) (second type) (nth type 2))
                              (second left) (second right))
        (if (first left) 1 -1))

      (= :variant (first type))
      (let [cases (nth type 2)
            indexes (zipmap (map first cases) (range))
            left-index (get indexes (second left))
            right-index (get indexes (second right))]
        (if (= left-index right-index)
          (compare-typed-values (second (nth cases left-index))
                                (nth left 2) (nth right 2))
          (compare left-index right-index)))

      (= :vector (first type))
      (compare-sequences (second type) (rest left) (rest right))

      (= :list (first type))
      (compare-sequences
       (repeat (max (count (second left)) (count (second right)))
               (second type))
       (second left) (second right))

      (= :set (first type))
      (compare-sequences (repeat (max (count (second left)) (count (second right)))
                                 (second type))
                         (second left) (second right))

      (= :map (first type))
      (let [entry-type [:vector [(second type) (nth type 2)]]]
        (compare-sequences (repeat (max (count (second left)) (count (second right)))
                                   entry-type)
                           (mapv #(into [entry-type] %) (second left))
                           (mapv #(into [entry-type] %) (second right))))

      (= :record (first type))
      (compare-sequences (map second (nth type 2)) (rest left) (rest right))

      :else (throw (ex-info "value type has no canonical order"
                            {:phase :value :type type})))))
(defn bounded-typed-value!
  "Validate a value under a canonical possibly-parametric type descriptor.
  Recursive values share fixed depth, node, and aggregate indirect-byte
  budgets."
  ([type value]
   (validate-value-type! type)
   (bounded-typed-value! type value 0 (volatile! 0)
                         (volatile! 0) (volatile! 0)))
  ([type value depth nodes]
   (bounded-typed-value! type value depth nodes
                         (volatile! 0) (volatile! 0)))
  ([type value depth nodes indirect-bytes]
   (bounded-typed-value! type value depth nodes indirect-bytes (volatile! 0)))
  ([type value depth nodes indirect-bytes list-items]
   (vswap! nodes inc)
   (when (> @nodes adt-node-limit)
     (throw (ex-info "ADT value exceeds node limit" {:phase :value :limit adt-node-limit})))
   (when (> depth adt-depth-limit)
     (throw (ex-info "ADT value exceeds depth limit" {:phase :value :limit adt-depth-limit})))
   (letfn [(charge-indirect! [bytes]
             (vswap! indirect-bytes + bytes)
             (when (> @indirect-bytes canonical-indirect-byte-limit)
               (throw
                (ex-info "canonical value exceeds aggregate indirect byte limit"
                         {:phase :value :bytes @indirect-bytes
                          :limit canonical-indirect-byte-limit}))))]
   (case type
     :i64 (do (when-not #?(:clj (and (integer? value) (<= Long/MIN_VALUE value Long/MAX_VALUE))
                              :cljs (and (i64/bigint-value? value) (i64/in-i64-range? value)))
                (throw (ex-info "value is not a signed i64" {:phase :value}))) value)
     :f64 (do (when-not (f64-value? value)
                (throw (ex-info "value is not f64" {:phase :value}))) value)
     :f32 (do (when-not (f32-value? value)
                (throw (ex-info "value is not f32" {:phase :value}))) value)
     :string (let [validated (bounded-string! value string-value-byte-limit)]
               (charge-indirect! (utf8-byte-count! validated))
               validated)
     :keyword (let [validated (bounded-keyword! value keyword-value-byte-limit)]
                (charge-indirect! (utf8-byte-count! (str validated)))
                validated)
     :symbol (bounded-symbol! value symbol-value-byte-limit)
     :map (bounded-map! value)
     :bool (do (when-not (boolean? value)
                 (throw (ex-info "value is not a boolean" {:phase :value}))) value)
     :option-i64 (bounded-option-i64! value)
     :result-i64 (bounded-result-i64! value)
     :vector-i64 (bounded-vector-i64! value)
     :vector-f64 (bounded-vector-f64! value)
     :string-index (bounded-string-index! value)
     :disjoint-set-i64 (bounded-disjoint-set-i64! value)
     :document (bounded-document! value)
     (cond
       (= :result (first type))
       (do
         (when-not (and (vector? value) (= 2 (count value)) (boolean? (first value)))
           (throw (ex-info "value is not a parametric result" {:phase :value})))
         (let [payload-type (if (first value) (second type) (nth type 2))]
           [(first value) (bounded-typed-value! payload-type (second value)
                                                (inc depth) nodes indirect-bytes
                                                list-items)]))

       (= :variant (first type))
       (do
         (when-not (and (vector? value) (= 3 (count value)) (= type (first value))
                        (keyword? (second value)))
           (throw (ex-info "value is not the declared variant type" {:phase :value})))
         (let [tag (second value)
               payload-type (some (fn [[case-tag case-type]]
                                    (when (= case-tag tag) case-type))
                                  (nth type 2))]
           (when-not payload-type
             (throw (ex-info "variant case is not declared" {:phase :value :tag tag})))
           [type tag (bounded-typed-value! payload-type (nth value 2)
                                           (inc depth) nodes indirect-bytes
                                           list-items)]))

       (= :option (first type))
       (do
         (when-not (and (vector? value) (= type (first value))
                        (or (and (= 2 (count value)) (false? (second value)))
                            (and (= 3 (count value)) (true? (second value)))))
           (throw (ex-info "value is not the declared generic option type" {:phase :value})))
         (if (false? (second value))
           [type false]
           [type true (bounded-typed-value! (second type) (nth value 2)
                                            (inc depth) nodes indirect-bytes
                                            list-items)]))

       (= :vector (first type))
       (let [item-types (second type)]
         (when-not (and (vector? value) (= type (first value))
                        (= (count value) (inc (count item-types))))
           (throw (ex-info "value is not the declared heterogeneous vector type"
                           {:phase :value})))
         (into [type]
               (map (fn [item-type item]
                      (bounded-typed-value! item-type item (inc depth)
                                            nodes indirect-bytes list-items))
                    item-types (rest value))))

       (= :list (first type))
       (let [item-type (second type)]
         (when-not (and (vector? value) (= 2 (count value))
                        (= type (first value))
                        (vector? (second value))
                        (<= (count (second value)) canonical-list-item-limit))
           (throw (ex-info "value is not the declared bounded list"
                           {:phase :value :limit canonical-list-item-limit})))
         (vswap! list-items + (count (second value)))
         (when (> @list-items canonical-list-total-item-limit)
           (throw
            (ex-info "canonical value exceeds aggregate list item limit"
                     {:phase :value :items @list-items
                      :limit canonical-list-total-item-limit})))
         [type
          (mapv #(bounded-typed-value! item-type % (inc depth)
                                       nodes indirect-bytes list-items)
                (second value))])

       (= :set (first type))
       (let [item-type (second type)]
         (when-not (and (vector? value) (= 2 (count value)) (= type (first value))
                        (vector? (second value))
                        (<= (count (second value)) typed-set-item-limit))
           (throw (ex-info "value is not the declared typed set"
                           {:phase :value :limit typed-set-item-limit})))
         (let [items (mapv #(bounded-typed-value! item-type % (inc depth)
                                                  nodes indirect-bytes list-items)
                           (second value))
               sorted-items (vec (sort #(compare-typed-values item-type %1 %2) items))]
           (when (some (fn [[left right]]
                         (zero? (compare-typed-values item-type left right)))
                       (partition 2 1 sorted-items))
             (throw (ex-info "typed set contains a duplicate item" {:phase :value})))
           [type sorted-items]))

       (= :map (first type))
       (let [key-type (second type)
             value-type (nth type 2)]
         (when-not (and (vector? value) (= 2 (count value)) (= type (first value))
                        (vector? (second value))
                        (<= (count (second value)) typed-map-entry-limit)
                        (every? #(and (vector? %) (= 2 (count %))) (second value)))
           (throw (ex-info "value is not the declared typed map"
                           {:phase :value :limit typed-map-entry-limit})))
         (let [entries (mapv (fn [[key item]]
                               [(bounded-typed-value! key-type key (inc depth)
                                                      nodes indirect-bytes list-items)
                                (bounded-typed-value! value-type item (inc depth)
                                                      nodes indirect-bytes list-items)])
                             (second value))
               sorted-entries (vec (sort #(compare-typed-values key-type
                                                                 (first %1) (first %2))
                                         entries))]
           (when (some (fn [[[left _] [right _]]]
                         (zero? (compare-typed-values key-type left right)))
                       (partition 2 1 sorted-entries))
             (throw (ex-info "typed map contains a duplicate key" {:phase :value})))
           [type sorted-entries]))

       (= :record (first type))
       (let [fields (nth type 2)]
         (when-not (and (vector? value) (= type (first value))
                        (= (count value) (inc (count fields))))
           (throw (ex-info "value is not the declared record type" {:phase :value})))
         (into [type]
               (map (fn [[_ field-type] field-value]
                      (bounded-typed-value! field-type field-value (inc depth)
                                            nodes indirect-bytes list-items))
                    fields (rest value))))

       (schema-ref-type? type)
       (bounded-typed-value! (resolve-schema-ref-type type value) value
                             depth nodes indirect-bytes list-items)

       :else (throw (ex-info "value type is outside the safe profile"
                             {:phase :value})))))))
