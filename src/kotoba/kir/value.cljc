(ns kotoba.kir.value
  (:require [clojure.string :as str]
            #?@(:cljs [[sha2.core :as sha2]
                       [kotoba.kir.cljs-i64 :as i64]]))
  #?(:clj (:import [java.nio.charset StandardCharsets]
                   [java.security MessageDigest])))

(def string-literal-byte-limit 4096)
(def string-value-byte-limit 65536)
;; Opaque binary payload bound for kit/runtime `:bytes` (stream-object
;; max-pull-bytes). Same magnitude as string-value-byte-limit deliberately.
(def bytes-value-byte-limit 65536)
(def keyword-value-byte-limit 512)
(def symbol-value-byte-limit 512)
(def map-entry-limit 128)
(def vector-literal-item-limit 128)
;; How many items one `vector-i64` / `vector-f64` may hold.
;;
;; 16,384 for as long as every element write COPIED the whole vector: the
;; native host memmoved and bump-allocated, the Wasm host did `[...checked]`
;; and froze, and `assertValue` re-scanned every element on every operation.
;; Under that, a bigger vector only bought a bigger copy -- raising this would
;; have made programs slower rather than possible.
;;
;; `vector-assoc!` removes the copy and the linear-value scan skip removes the
;; per-operation walk (superproject ADR-2609010200), so the number is now set
;; by what a guest needs rather than by what a copy costs.
;;
;; 1,048,576 because that is `torihiki.book`'s default `cap` -- one slot per
;; resting order in a struct of arrays. Chosen against a real program rather
;; than rounded up: a limit nobody's workload explains is a limit nobody can
;; argue with when it turns out to be wrong.
;;
;; Still a limit and still fail-closed. 8 MB per i64 field is a real cost, and
;; a guest asking for more is refused rather than allowed to exhaust the host.
(def vector-item-limit 16384)
;; Raised to 1048576 for the order book above, then put back on 2026-09-01.
;; The in-place lowering had to land first -- raising the ceiling before it
;; only makes each copy bigger -- and it now has, so the argument above is no
;; longer premature. It is still a SEPARATE decision: lang/surface-status.edn
;; records :arena-bounds as :not-met with its own fail-closed argument owing,
;; and amu's component test asserts that a length one past this traps in the
;; core. Raising it silently inside the lowering change made that test pass
;; for the wrong reason -- the guest was admitted, not refused -- which is
;; the shape this file exists to prevent. Raise it with the native arena, the
;; host budget and that test in one change that argues for the number.
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
;; Raised 8 → 12 for T8.3/W4 structured key/value pair spines in recursive
;; EDN ADTs (pair-of-entry-pairs for header maps + request/result spines).
;; Document depth stays at document-depth-limit 8. ADR kotoba-kir 0025.
(def adt-depth-limit 12)
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

(defn bytes-value?
  "Host representation of kit/runtime `:bytes`: a raw byte array on JVM,
  a `Uint8Array` on cljs/nbb. Not a string — UTF-8 is not assumed."
  [value]
  #?(:clj (bytes? value)
     :cljs (instance? js/Uint8Array value)))

(defn bytes-byte-count
  "Length in bytes of a host `:bytes` value. Throws if not bytes."
  [value]
  (when-not (bytes-value? value)
    (throw (ex-info "value is not bytes" {:phase :value :value value})))
  #?(:clj (alength ^bytes value)
     :cljs (.-byteLength value)))

(defn bounded-bytes!
  "Admit a host `:bytes` value whose length is ≤ `limit` (default
  `bytes-value-byte-limit`). Returns the same object (no copy)."
  ([value] (bounded-bytes! value bytes-value-byte-limit))
  ([value limit]
   (let [n (bytes-byte-count value)]
     (when-not (and (integer? limit) (<= 0 limit))
       (throw (ex-info "bytes limit is invalid" {:phase :value :limit limit})))
     (when (> n limit)
       (throw (ex-info "bytes value exceeds byte limit"
                       {:phase :value :bytes n :limit limit})))
     value)))

(defn utf8-string->bytes
  "Construct a host `:bytes` value from a UTF-8 string. Fixture/test helper
  and host bridge — not a guest language op."
  [s]
  (when-not (string? s)
    (throw (ex-info "utf8-string->bytes requires a string" {:phase :value})))
  #?(:clj (.getBytes ^String s StandardCharsets/UTF_8)
     :cljs (.encode (js/TextEncoder.) s)))


(def ^:private handle-seq (atom 0))

(defn- next-handle-id!
  []
  (swap! handle-seq inc))

;; ---------------------------------------------------------------------------
;; Linear resource table (ADR 0133)
;; Host-side registry for affine task/stream handles. Construction registers;
;; ops require :alive; drop! clears. Use-after-drop and double-drop fail closed.
;; This is the reference dual-runtime ownership plane — not the Component Model
;; resource table ABI (wasm packaging still intermediate, ADR 0130/0131).
;; ---------------------------------------------------------------------------

(def ^:private resource-table
  (atom {:tasks {} :streams {}}))

(defn resource-table-reset!
  "Test helper: clear the linear resource table and handle id sequence."
  []
  (reset! resource-table {:tasks {} :streams {}})
  (reset! handle-seq 0)
  nil)

(defn- register-stream!
  [stream]
  (swap! resource-table assoc-in [:streams (:kotoba.stream/id stream)]
         {:alive true})
  stream)

(defn- register-task!
  [task]
  (when-let [s (:kotoba.task/stream task)]
    (register-stream! s))
  (swap! resource-table assoc-in [:tasks (:kotoba.task/id task)]
         {:alive true
          :state (:kotoba.task/state task)
          :stream (:kotoba.task/stream task)})
  task)

(defn stream-value?
  "Host affine handle for [:stream :bytes]."
  [value]
  (and (map? value)
       (contains? value :kotoba.stream/id)
       (= :bytes (:kotoba.stream/item-type value))
       (bytes-value? (:kotoba.stream/payload value))
       (some? (:kotoba.stream/state value))))

(defn task-value?
  "Host affine handle for [:task [:stream :bytes]]."
  [value]
  (and (map? value)
       (contains? value :kotoba.task/id)
       (= [:stream :bytes] (:kotoba.task/result-type value))
       (contains? #{:pending :ready :cancelled} (:kotoba.task/state value))
       (let [s (:kotoba.task/stream value)]
         (or (nil? s) (stream-value? s)))))

(defn- require-stream-alive!
  [stream]
  (when-not (stream-value? stream)
    (throw (ex-info "not a bytes-stream" {:phase :value})))
  (let [id (:kotoba.stream/id stream)
        e (get-in @resource-table [:streams id])]
    (when-not (and e (:alive e))
      (throw (ex-info "bytes-stream is not live"
                      {:phase :value :id id})))))

(defn- require-task-alive!
  [task]
  (when-not (task-value? task)
    (throw (ex-info "not a bytes-task" {:phase :value})))
  (let [id (:kotoba.task/id task)
        e (get-in @resource-table [:tasks id])]
    (when-not (and e (:alive e))
      (throw (ex-info "bytes-task is not live"
                      {:phase :value :id id})))
    e))

(defn stream-drop!
  "Linear drop of a bytes-stream. Subsequent ops on this id fail closed.
  Double-drop fails closed."
  [stream]
  (require-stream-alive! stream)
  (swap! resource-table assoc-in [:streams (:kotoba.stream/id stream) :alive] false)
  nil)

(defn task-drop!
  "Linear drop of a bytes-task (and its ready stream if any). Subsequent ops
  on these ids fail closed. Double-drop fails closed."
  [task]
  (let [entry (require-task-alive! task)]
    (when-let [s (:stream entry)]
    (when (get-in @resource-table [:streams (:kotoba.stream/id s) :alive])
        (stream-drop! s))))
  (swap! resource-table assoc-in [:tasks (:kotoba.task/id task) :alive] false)
  nil)

(defn stream-live?
  "True when stream id is registered and alive in the resource table."
  [stream]
  (boolean (and (stream-value? stream)
                (get-in @resource-table [:streams (:kotoba.stream/id stream) :alive]))))

(defn task-live?
  "True when task id is registered and alive in the resource table."
  [task]
  (boolean (and (task-value? task)
                (get-in @resource-table [:tasks (:kotoba.task/id task) :alive]))))

(defn make-bytes-stream
  "Construct a host [:stream :bytes] value over a single payload chunk.
  Registers the handle in the linear resource table (ADR 0133)."
  [payload]
  (let [payload (bounded-bytes! payload)]
    (register-stream!
     {:kotoba.stream/id (next-handle-id!)
      :kotoba.stream/item-type :bytes
      :kotoba.stream/payload payload
      :kotoba.stream/state (atom {:offset 0 :cancelled? false})})))

(defn concat-bytes
  "Concatenate a sequence of host :bytes values into one bounded payload."
  [chunks]
  (when-not (and (sequential? chunks) (seq chunks))
    (throw (ex-info "concat-bytes requires a non-empty sequence of bytes"
                    {:phase :value})))
  (let [chunks (mapv bounded-bytes! chunks)
        total (reduce + 0 (map bytes-byte-count chunks))]
    (when (> total bytes-value-byte-limit)
      (throw (ex-info "concatenated bytes exceed byte limit"
                      {:phase :value :bytes total :limit bytes-value-byte-limit})))
    #?(:clj
       (let [out (byte-array total)]
         (loop [i 0 xs chunks]
           (if (empty? xs)
             out
             (let [^bytes c (first xs)
                   n (alength c)]
               (System/arraycopy c 0 out i n)
               (recur (+ i n) (rest xs))))))
       :cljs
       (let [out (js/Uint8Array. total)]
         (loop [i 0 xs (seq chunks)]
           (if xs
             (let [c (first xs)
                   n (.-byteLength c)]
               (.set out c i)
               (recur (+ i n) (next xs)))
             out))))))

(defn make-bytes-stream-from-chunks
  "Build a bytes-stream by concatenating ordered chunks (multi-chunk producer
  first slice — single linear payload after join; ADR 0123)."
  [chunks]
  (make-bytes-stream (concat-bytes chunks)))

(defn empty-bytes
  "Construct the canonical empty `:bytes` value for the current host."
  []
  #?(:clj (byte-array 0) :cljs (js/Uint8Array. 0)))

(defn make-chunk-queue-bytes-stream
  "True multi-chunk producer stream (ADR 0125): each `stream-read!` yields one
  producer chunk without pre-joining. A single producer chunk that exceeds
  `max-bytes` fails closed (atomic chunks). Empty remaining queue → done
  (closed queue; not progressive)."
  [chunks]
  (when-not (and (sequential? chunks) (seq chunks))
    (throw (ex-info "chunk-queue requires a non-empty sequence of bytes"
                    {:phase :value})))
  (let [chunks (mapv bounded-bytes! chunks)
        total (reduce + 0 (map bytes-byte-count chunks))]
    (when (> total bytes-value-byte-limit)
      (throw (ex-info "chunk-queue total bytes exceed byte limit"
                      {:phase :value :bytes total :limit bytes-value-byte-limit})))
    (register-stream!
     {:kotoba.stream/id (next-handle-id!)
      :kotoba.stream/item-type :bytes
      ;; Linear payload unused; kept empty so stream-value? stays uniform.
      :kotoba.stream/payload (empty-bytes)
      :kotoba.stream/state (atom {:mode :chunk-queue
                                  :queue (vec chunks)
                                  :open? false
                                  :enqueued-bytes total
                                  :cancelled? false})})))

(defn make-open-chunk-queue-bytes-stream
  "Progressive multi-chunk stream (ADR 0126): starts empty and open.
  Producer `stream-enqueue!` while consumer may `stream-read!`;
  `stream-close!` ends the stream. Empty+open reads return
  `{:pending? true :done? false}` (non-blocking poll)."
  []
  (register-stream!
   {:kotoba.stream/id (next-handle-id!)
    :kotoba.stream/item-type :bytes
    :kotoba.stream/payload (empty-bytes)
    :kotoba.stream/state (atom {:mode :chunk-queue
                                :queue []
                                :open? true
                                :enqueued-bytes 0
                                :cancelled? false})}))

(defn make-ready-bytes-task
  "Construct a host [:task [:stream :bytes]] already :ready with one stream.
  Registers the task (and stream) in the linear resource table (ADR 0133)."
  [payload]
  (register-task!
   {:kotoba.task/id (next-handle-id!)
    :kotoba.task/result-type [:stream :bytes]
    :kotoba.task/state :ready
    :kotoba.task/stream (make-bytes-stream payload)}))

(defn make-ready-bytes-task-from-chunk-queue
  "Ready task whose stream yields each producer chunk on successive reads
  (true multi-chunk; ADR 0125)."
  [chunks]
  (register-task!
   {:kotoba.task/id (next-handle-id!)
    :kotoba.task/result-type [:stream :bytes]
    :kotoba.task/state :ready
    :kotoba.task/stream (make-chunk-queue-bytes-stream chunks)}))

(defn make-ready-open-chunk-queue-task
  "Ready task with an open progressive stream (ADR 0126). Host enqueues then
  closes; consumer polls stream-read! for pending/ready/done."
  []
  (register-task!
   {:kotoba.task/id (next-handle-id!)
    :kotoba.task/result-type [:stream :bytes]
    :kotoba.task/state :ready
    :kotoba.task/stream (make-open-chunk-queue-bytes-stream)}))

(defn make-pending-bytes-task
  "Construct a host bytes-task still :pending (no stream yet)."
  []
  (register-task!
   {:kotoba.task/id (next-handle-id!)
    :kotoba.task/result-type [:stream :bytes]
    :kotoba.task/state :pending
    :kotoba.task/stream nil}))

(defn task-poll
  "Poll a bytes-task. Returns {:state :ready|:pending|:cancelled :stream s?}.
  The resource table is authoritative, so a host fulfillment is observable
  through the original pending handle. Requires the task to be live."
  [task]
  (let [{:keys [state stream]} (require-task-alive! task)]
    (case state
    :ready {:state :ready :stream stream}
    :pending {:state :pending}
      :cancelled {:state :cancelled})))

(defn task-cancel!
  "Cancel the task in the authoritative resource table and return a compatible
  snapshot. Every copy of the same affine handle observes cancellation."
  [task]
  (let [{:keys [stream]} (require-task-alive! task)
        id (:kotoba.task/id task)]
    (swap! resource-table assoc-in [:tasks id :state] :cancelled)
    (assoc task :kotoba.task/state :cancelled :kotoba.task/stream stream)))

(defn- fulfill-task-with-stream!
  [task stream]
  (let [id (:kotoba.task/id task)]
    (try
      (swap! resource-table update-in [:tasks id]
             (fn [{:keys [alive state] :as entry}]
               (when-not alive
                 (throw (ex-info "bytes-task is not live"
                                 {:phase :value :id id})))
               (when-not (= :pending state)
                 (throw (ex-info "task is not pending"
                                 {:phase :value :state state})))
               (assoc entry :state :ready :stream stream)))
      (assoc task :kotoba.task/state :ready :kotoba.task/stream stream)
      (catch #?(:clj Throwable :cljs :default) error
        ;; Stream construction registers first. A losing/racing transition must
        ;; not leak an unreachable affine resource.
        (when (stream-live? stream)
          (stream-drop! stream))
        (throw error)))))

(defn task-fulfill!
  "Transition a :pending bytes-task to :ready with a stream over `payload`.
  The resource table owns the transition, so polling the original handle sees
  readiness. Returns a same-id snapshot for compatibility."
  [task payload]
  (require-task-alive! task)
  (fulfill-task-with-stream! task (make-bytes-stream payload)))

(defn task-fulfill-chunk-queue!
  "Fulfill a pending task with a true multi-chunk stream (ADR 0125).
  Same id; fails closed if not pending."
  [task chunks]
  (require-task-alive! task)
  (fulfill-task-with-stream! task (make-chunk-queue-bytes-stream chunks)))

(defn task-fulfill-open-chunk-queue!
  "Fulfill a pending task with an open progressive stream (ADR 0126)."
  [task]
  (require-task-alive! task)
  (fulfill-task-with-stream! task (make-open-chunk-queue-bytes-stream)))

(defn stream-cancel!
  "Cancel a bytes-stream. Subsequent reads fail closed."
  [stream]
  (require-stream-alive! stream)
  (swap! (:kotoba.stream/state stream) assoc :cancelled? true)
  stream)

(defn stream-enqueue!
  "Push one producer chunk onto an open chunk-queue stream (ADR 0126).
  Fails closed if not chunk-queue, cancelled, or already closed. Total
  enqueued bytes across the stream lifetime share `bytes-value-byte-limit`."
  [stream chunk]
  (require-stream-alive! stream)
  (let [st (:kotoba.stream/state stream)
        snap @st
        payload (bounded-bytes! chunk)
        n (bytes-byte-count payload)]
    (when-not (= :chunk-queue (:mode snap))
      (throw (ex-info "stream-enqueue! requires chunk-queue mode"
                      {:phase :value})))
    (when (:cancelled? snap)
      (throw (ex-info "bytes-stream is cancelled" {:phase :value})))
    (when-not (:open? snap)
      (throw (ex-info "bytes-stream is closed; cannot enqueue"
                      {:phase :value})))
    (let [total (+ (long (:enqueued-bytes snap 0)) n)]
      (when (> total bytes-value-byte-limit)
        (throw (ex-info "chunk-queue total enqueued bytes exceed byte limit"
                        {:phase :value :bytes total :limit bytes-value-byte-limit})))
      (swap! st (fn [s]
                  (when-not (and (= :chunk-queue (:mode s))
                                 (:open? s)
                                 (not (:cancelled? s)))
                    (throw (ex-info "bytes-stream is not open for enqueue"
                                    {:phase :value})))
                  (assoc s
                         :queue (conj (vec (:queue s)) payload)
                         :enqueued-bytes (+ (long (:enqueued-bytes s 0)) n))))
      stream)))

(defn stream-close!
  "Close an open chunk-queue stream (ADR 0126). Further enqueue fails;
  reads drain remaining queue then complete with done?."
  [stream]
  (require-stream-alive! stream)
  (let [st (:kotoba.stream/state stream)
        snap @st]
    (when-not (= :chunk-queue (:mode snap))
      (throw (ex-info "stream-close! requires chunk-queue mode"
                      {:phase :value})))
    (when (:cancelled? snap)
      (throw (ex-info "bytes-stream is cancelled" {:phase :value})))
    (swap! st assoc :open? false)
    stream))

(defn stream-read!
  "Pull up to max-bytes from a bytes-stream.
  Returns {:bytes <bytes> :done? bool :pending? bool?}.
  Linear mode (default): splits a single payload by max-bytes.
  Chunk-queue mode (ADR 0125/0126): yields one whole producer chunk per call;
  a producer chunk larger than max-bytes fails closed.
  Open progressive queue (ADR 0126): empty+open → {:pending? true :done? false};
  empty+closed → done."
  [stream max-bytes]
  (require-stream-alive! stream)
  (when-not (and (integer? max-bytes) (pos? max-bytes)
                 (<= max-bytes bytes-value-byte-limit))
    (throw (ex-info "stream-read max-bytes out of range"
                    {:phase :value :max-bytes max-bytes})))
  (let [st (:kotoba.stream/state stream)]
    (when (:cancelled? @st)
      (throw (ex-info "bytes-stream is cancelled" {:phase :value})))
    (if (= :chunk-queue (:mode @st))
      (let [snap @st
            q (:queue snap)
            open? (boolean (:open? snap))]
        (if (empty? q)
          (if open?
            {:bytes (empty-bytes) :done? false :pending? true}
            {:bytes (empty-bytes) :done? true :pending? false})
          (let [head (first q)
                n (bytes-byte-count head)]
            (when (> n max-bytes)
              (throw (ex-info "chunk-queue producer chunk exceeds max-bytes"
                              {:phase :value :chunk-bytes n :max-bytes max-bytes})))
            (let [rest-q (vec (rest q))]
              (swap! st assoc :queue rest-q)
              ;; re-check open? after dequeue (producer may still be open)
              (let [still-open? (boolean (:open? @st))]
                {:bytes head
                 :done? (and (empty? rest-q) (not still-open?))
                 :pending? false})))))
      (let [payload (:kotoba.stream/payload stream)
            total (bytes-byte-count payload)
            offset (:offset @st 0)
            remain (- total offset)]
        (if (<= remain 0)
          {:bytes (empty-bytes) :done? true :pending? false}
          (let [n (min remain max-bytes)
                chunk #?(:clj (java.util.Arrays/copyOfRange ^bytes payload (int offset) (int (+ offset n)))
                         :cljs (.slice payload offset (+ offset n)))
                next-offset (+ offset n)
                done? (>= next-offset total)]
            (swap! st assoc :offset next-offset)
            {:bytes chunk :done? done? :pending? false}))))))

(defn- bounded-host-byte-offset
  "Normalize a guest i64 byte offset to a host string index only after it is
  proven inside the small UTF-8 payload bound. CLJS guest integers are BigInt;
  comparing them with the host-number byte length makes valid offsets fail and
  using them as map keys misses number-keyed boundary tables."
  [offset length inclusive-end?]
  #?(:clj
     (when (and (integer? offset) (<= 0 offset)
                ((if inclusive-end? <= <) offset length))
       (long offset))
     :cljs
     (let [offset* (cond
                     (i64/bigint-value? offset) offset
                     (integer? offset) (i64/->bigint offset)
                     :else nil)
           length* (i64/->bigint length)]
       (when (and offset* (not (i64/k-neg? offset*))
                  ((if inclusive-end? <= <) offset* length*))
         (js/Number offset*)))))

(defn utf8-substring!
  "Checked UTF-8 byte-offset substring. Both offsets must be code-point
  boundaries; malformed UTF-16 is rejected by utf8-byte-count! first."
  [value start end]
  (let [length (utf8-byte-count! value)
        start-index (bounded-host-byte-offset start length true)
        end-index (bounded-host-byte-offset end length true)]
    (when-not (and (some? start-index) (some? end-index)
                   (<= start-index end-index))
      (throw (ex-info "string substring indexes are out of bounds"
                      {:phase :value :start start :end end :length length})))
    (loop [index 0 byte-index 0 boundaries {0 0}]
      (if (= index (count value))
        (let [from (get boundaries start-index) to (get boundaries end-index)]
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

(defn utf8-index-of!
  "First UTF-8 BYTE offset of NEEDLE in VALUE, -1 when absent. The empty
  needle is refused by the caller (KIR traps on it), mirroring
  :empty-string-search-needle. Strings are CLJC strings here; the byte
  offset is derived by walking code-point boundaries exactly as
  utf8-substring! does, so astral and multi-byte prefixes compose."
  [value needle]
  (let [_check (utf8-byte-count! value)
        _check-needle (utf8-byte-count! needle)]
    (if-let [host-idx #?(:clj (.indexOf ^String value ^String needle)
                         :cljs (.indexOf value needle))]
      (if (neg? host-idx)
        -1
        ;; host index is a UTF-16 unit index; convert to UTF-8 byte offset.
        (loop [i 0 byte-index 0]
          (if (>= i host-idx)
            byte-index
            (let [unit #?(:clj (int (.charAt ^String value i))
                          :cljs (.charCodeAt value i))
                  bytes (cond
                          (<= unit 0x7f) 1
                          (<= unit 0x7ff) 2
                          (<= 0xd800 unit 0xdbff) 4
                          :else 3)]
              (recur (+ i 1) (+ byte-index bytes))))))
      -1)))

(defn utf8-code-point-at!
  "Return the Unicode code point of the UTF-8 sequence that STARTS at BYTE-OFFSET
  (a UTF-8 byte offset into VALUE, same coordinate space as utf8-substring!'s
  offsets and string-byte-length). BYTE-OFFSET must be a code-point boundary in
  [0, byte-length); anything else traps. The guest can derive the code point's
  UTF-8 byte width from the returned value (< 0x80 -> 1, < 0x800 -> 2,
  < 0x10000 -> 3, else 4) to advance, so a single op is enough to walk a string."
  [value byte-offset]
  (let [length (utf8-byte-count! value)
        byte-index-target (bounded-host-byte-offset byte-offset length false)]
    (when-not (some? byte-index-target)
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
          (= byte-index byte-index-target) code-point
          (> (+ byte-index bytes) byte-index-target)
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

;; The mirror of `fold-case!`. The same locale determinism rules apply: the
;; JVM `clojure.string/upper-case` (and bare `.toUpperCase()`) folds through
;; the platform DEFAULT locale -- Turkish (`tr`/`tr-TR`) maps lowercase `i` to
;; dotted capital `İ`, not `I` -- so this always pins `Locale/ROOT` on the
;; JVM. cljs's `.toUpperCase()` (no-arg) is already locale-independent
;; Unicode simple case mapping. The two are verified to agree on the ASCII and
;; common accented-Latin ranges this primitive's conformance vectors cover;
;; the full Unicode SpecialCasing table (Turkish `İ`/`ı`, German `ß`, Greek
;; final-sigma context, Lithuanian dot-retention) is explicitly out of scope,
;; exactly as for `fold-case!`.
(defn upper-case!
  [value]
  (when-not (string? value)
    (throw (ex-info "value is not a string" {:phase :value :value value})))
  #?(:clj (.toUpperCase ^String value java.util.Locale/ROOT)
     :cljs (.toUpperCase value)))

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

(declare document-canonical-bytes document-compare document-map-key-compare)

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
    (letfn [(walk-map-key [key depth]
              ;; Keyword keys existed before keys became document nodes. Keep
              ;; their historical text budget without consuming a node slot.
              (cond
                (keyword? key)
                (do (bounded-keyword! key keyword-value-byte-limit)
                    (charge-text! (str key))
                    ["keyword" key])

                (and (vector? key) (= 2 (count key)) (= "keyword" (first key)))
                (let [payload (second key)]
                  (bounded-keyword! payload keyword-value-byte-limit)
                  (charge-text! (str payload))
                  ["keyword" payload])

                :else (walk key depth)))
            (walk [node depth]
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
                  "symbol" [tag (do (bounded-symbol! payload symbol-value-byte-limit)
                                     (charge-text! (str payload)) payload)]
                  ("vector" "list")
                  (do (when-not (and (vector? payload)
                                     (<= (count payload) document-container-item-limit))
                        (throw (ex-info (str "invalid document " tag)
                                        {:phase :value :limit document-container-item-limit})))
                      [tag (mapv #(walk % (inc depth)) payload)])
                  "set"
                  (do (when-not (and (vector? payload)
                                     (<= (count payload) document-container-item-limit))
                        (throw (ex-info "invalid document set"
                                        {:phase :value :limit document-container-item-limit})))
                      (let [items (mapv #(walk % (inc depth)) payload)
                            canonical (vec (sort document-compare items))]
                        (when-not (and (= items canonical)
                                       (every? (fn [[left right]]
                                                 (neg? (document-compare left right)))
                                               (partition 2 1 items)))
                          (throw (ex-info "document set items are duplicate or noncanonical"
                                          {:phase :value})))
                        [tag items]))
                  "map"
                  (do (when-not (and (vector? payload)
                                     (<= (count payload) document-container-item-limit)
                                     (every? #(and (vector? %) (= 2 (count %))) payload))
                        (throw (ex-info "invalid document map"
                                        {:phase :value :limit document-container-item-limit})))
                      (let [entries (mapv (fn [[key item]]
                                            [(walk-map-key key (inc depth))
                                             (walk item (inc depth))])
                                          payload)
                            canonical (vec (sort (fn [[left] [right]]
                                                   (document-map-key-compare left right))
                                                 entries))]
                        (when-not (and (= entries canonical)
                                       (every? (fn [[[left] [right]]]
                                                 (neg? (document-map-key-compare left right)))
                                               (partition 2 1 entries)))
                          (throw (ex-info "document map keys are duplicate or noncanonical"
                                          {:phase :value})))
                        [tag entries]))
                  (throw (ex-info "unknown document tag" {:phase :value :tag tag})))))]
      (walk value 0))))

(defn- utf8-bytes
  "UTF-8 byte sequence for S as a seq of 0-255 ints."
  [s]
  #?(:clj (map #(bit-and (int %) 0xff) (.getBytes ^String s StandardCharsets/UTF_8))
     ;; nbb keeps typed arrays opaque under js->clj. Convert through a plain
     ;; JS Array so count/doseq behave identically on JVM Clojure and CLJS.
     :cljs (js->clj (js/Array.from (.encode (js/TextEncoder.) s)))))

(defn- normalize-document-f64 [value]
  ;; Match document-equal? identity for signed zero.
  (if (zero? value) 0.0 value))

(defn document-canonical-bytes
  "Deterministic UTF-8 identity encoding of a validated document. Format:
  n | b t/f | i <decimal> ; | f <i64-bits-decimal> ; |
  s <utf8-len> : <bytes> | k <utf8-len> : <keyword-str-with-colon-bytes> |
  y <utf8-len> : <symbol-bytes> |
  v <count> : <items...> | l <count> : <items...> | e <count> : <items...> |
  m <count> : ((K <keyword-len> : <keyword-bytes> | D <document-key>) <item>)*

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
            (ascii [ch]
              #?(:clj (int ch) :cljs (.charCodeAt ch 0)))
            (emit-str [s]
              (doseq [b (utf8-bytes s)] (emit b)))
            (emit-len-str [s]
              (let [bs (utf8-bytes s)]
                (emit-str (str (count bs)))
                (emit (ascii \:))
                (doseq [b bs] (emit b))))
            (walk [node]
              (let [tag (first node)]
                (case tag
                  "null" (emit (ascii \n))
                  "bool" (do (emit (ascii \b))
                             (emit (if (second node) (ascii \t) (ascii \f))))
                  "i64" (do (emit (ascii \i))
                            (emit-str (str (second node)))
                            (emit (ascii \;)))
                  "f64" (do (emit (ascii \f))
                            (emit-str (str (f64-to-i64-bits (normalize-document-f64 (second node)))))
                            (emit (ascii \;)))
                  "string" (do (emit (ascii \s))
                               (emit-len-str (second node)))
                  "keyword" (do (emit (ascii \k))
                                (emit-len-str (str (second node))))
                  "symbol" (do (emit (ascii \y))
                               (emit-len-str (str (second node))))
                  "vector" (do (emit (ascii \v))
                               (emit-str (str (count (second node))))
                               (emit (ascii \:))
                               (doseq [item (second node)] (walk item)))
                  "list" (do (emit (ascii \l))
                             (emit-str (str (count (second node))))
                             (emit (ascii \:))
                             (doseq [item (second node)] (walk item)))
                  "set" (do (emit (ascii \e))
                            (emit-str (str (count (second node))))
                            (emit (ascii \:))
                            (doseq [item (second node)] (walk item)))
                  "map" (do (emit (ascii \m))
                            (emit-str (str (count (second node))))
                            (emit (ascii \:))
                            (doseq [[k item] (second node)]
                              (if (= "keyword" (first k))
                                (do (emit (ascii \K))
                                    (emit-len-str (str (second k))))
                                (do (emit (ascii \D))
                                    (walk k)))
                              (walk item)))
                  (throw (ex-info "unknown document tag in canonical encoding"
                                  {:phase :value :tag tag})))))]
      (walk doc)
      #?(:clj (let [arr (byte-array (.size out))]
                (dotimes [i (.size out)]
                  (aset-byte arr i (unchecked-byte (.get out i))))
                arr)
         :cljs (js/Uint8Array.from out)))))

(defn document-compare
  "Total order for bounded documents, defined as unsigned lexicographic order
  of their canonical bytes. Set elements and general map keys share this one
  host-independent ordering substrate."
  [left right]
  (let [left-bytes (document-canonical-bytes left)
        right-bytes (document-canonical-bytes right)
        left-count #?(:clj (alength ^bytes left-bytes) :cljs (.-length left-bytes))
        right-count #?(:clj (alength ^bytes right-bytes) :cljs (.-length right-bytes))
        common (min left-count right-count)]
    (loop [index 0]
      (if (= index common)
        (compare left-count right-count)
        (let [left-byte #?(:clj (bit-and (int (aget ^bytes left-bytes index)) 0xff)
                            :cljs (aget left-bytes index))
              right-byte #?(:clj (bit-and (int (aget ^bytes right-bytes index)) 0xff)
                             :cljs (aget right-bytes index))]
          (if (= left-byte right-byte)
            (recur (inc index))
            (compare left-byte right-byte)))))))

(defn document-map-key-compare
  "Total order for document map keys. Keyword pairs retain the original
  keyword-text order so existing canonical keyword maps keep byte identity;
  every other pair uses the canonical document byte order."
  [left right]
  (if (and (= "keyword" (first left)) (= "keyword" (first right)))
    (compare (str (second left)) (str (second right)))
    (document-compare left right)))

(defn document-sha256-hex
  "SHA-256 hex digest of document-canonical-bytes. Host-independent identity
  for logical documents (W4 exit gate)."
  [value]
  (let [bytes (document-canonical-bytes value)]
    #?(:clj (let [digest (.digest (MessageDigest/getInstance "SHA-256") ^bytes bytes)]
              (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest)))
       ;; ClojureScript has no MessageDigest, and this threw rather than
       ;; digesting -- so `document-sha256`, the W4 exit-gate identity for a
       ;; logical document, was a JVM-only operation while every other document
       ;; operation was portable. `sha2.core` computes the same digest in
       ;; portable Clojure, so the two branches differ only in which
       ;; implementation they reach, not in what they return;
       ;; `document-sha256-agrees-across-hosts` pins that against `shasum`.
       ;;
       ;; NOT `multiformats.core/sha256`, which was the first version of this
       ;; and reads the same at a glance: on `:cljs` that namespace requires
       ;; `@noble/hashes/sha2.js`, so routing through it put an npm package in
       ;; the require graph of a T0 namespace. It went unnoticed because the
       ;; io-multiformats checkout on hand predated that require -- being
       ;; a direct dependency says nothing about what the resolved version of
       ;; it pulls in. org-nist-sha2 is the implementation multiformats itself
       ;; used, one link closer, with no host packages at all.
       :cljs (sha2/sha256-hex (mapv #(bit-and % 0xff) bytes)))))

(defn- bytes->hex
  "Lowercase hex of a byte array."
  [bytes]
  #?(:clj (let [^bytes arr bytes]
            (apply str (map #(format "%02x" (bit-and (int %) 0xff)) arr)))
     :cljs (apply str (map (fn [index]
                             (let [b (aget bytes index)
                                   h (.toString (bit-and b 255) 16)]
                               (if (< (count h) 2) (str "0" h) h)))
                           (range (.-length bytes))))))

(defn- hex->bytes!
  "Decode a lowercase hex string into a JVM byte-array (or cljs Uint8Array)."
  [s]
  (when-not (string? s)
    (throw (ex-info "document-read requires a string" {:phase :value})))
  (let [n (count s)]
    (when (odd? n)
      (throw (ex-info "document-read hex length must be even"
                      {:phase :value :length n})))
    (when (> n 1048576)
      (throw (ex-info "document-read hex exceeds size limit"
                      {:phase :value :length n})))
    (dotimes [i n]
      (let [code #?(:clj (int (.charAt ^String s i)) :cljs (.charCodeAt s i))
            ok (or (and (>= code 48) (<= code 57))
                   (and (>= code 97) (<= code 102)))]
        (when-not ok
          (throw (ex-info "document-read hex must be lowercase [0-9a-f]"
                          {:phase :value :index i})))))
    #?(:clj
       (let [arr (byte-array (quot n 2))]
         (dotimes [i (quot n 2)]
           (let [hi (Character/digit (.charAt ^String s (* 2 i)) 16)
                 lo (Character/digit (.charAt ^String s (inc (* 2 i))) 16)]
             (aset-byte arr i (unchecked-byte (+ (* hi 16) lo)))))
         arr)
       :cljs
       (let [arr (js/Uint8Array. (quot n 2))]
         (dotimes [i (quot n 2)]
           (let [hi (js/parseInt (.substring s (* 2 i) (inc (* 2 i))) 16)
                 lo (js/parseInt (.substring s (inc (* 2 i)) (+ 2 (* 2 i))) 16)]
             (aset arr i (+ (* hi 16) lo))))
         arr))))

(defn- document-from-canonical-bytes!
  "Inverse of document-canonical-bytes. Result still passes through
  bounded-document! for depth/node/item/byte budgets."
  [bytes]
  (let [len #?(:clj (alength ^bytes bytes) :cljs (.-length bytes))
        idx (volatile! 0)]
    (letfn [(peek-byte []
              (when (>= @idx len)
                (throw (ex-info "document-read truncated encoding"
                                {:phase :value :offset @idx})))
              #?(:clj (bit-and (int (aget ^bytes bytes @idx)) 0xff)
                 :cljs (aget bytes @idx)))
            (take-byte []
              (let [b (peek-byte)]
                (vswap! idx inc)
                b))
            (take-until [sep]
              (let [start @idx]
                (loop []
                  (when (>= @idx len)
                    (throw (ex-info "document-read missing terminator"
                                    {:phase :value :sep sep})))
                  (if (= (peek-byte) sep)
                    (let [end @idx]
                      (vswap! idx inc)
                      #?(:clj (String. ^bytes bytes start (- end start) StandardCharsets/UTF_8)
                         :cljs (.decode (js/TextDecoder. "utf-8")
                                        (.subarray bytes start end))))
                    (do (vswap! idx inc) (recur))))))
            (take-len-str []
              (let [len-str (take-until 58) ;; ':'
                    n (try
                        #?(:clj (Long/parseLong len-str)
                           :cljs (js/parseInt len-str 10))
                        (catch #?(:clj Exception :cljs :default) _
                          (throw (ex-info "document-read invalid length"
                                          {:phase :value :text len-str}))))]
                (when (or (neg? n) (> n document-utf8-byte-limit))
                  (throw (ex-info "document-read string length out of range"
                                  {:phase :value :length n})))
                (when (> (+ @idx n) len)
                  (throw (ex-info "document-read truncated string payload"
                                  {:phase :value :need n})))
                (let [start @idx]
                  (vswap! idx + n)
                  #?(:clj (String. ^bytes bytes start (int n) StandardCharsets/UTF_8)
                     :cljs (.decode (js/TextDecoder. "utf-8")
                                    (.subarray bytes start (+ start n)))))))
            (parse-int-decimal [s]
              (try
                #?(:clj (Long/parseLong s)
                   :cljs (js/BigInt s))
                (catch #?(:clj Exception :cljs :default) _
                  (throw (ex-info "document-read invalid integer"
                                  {:phase :value :text s})))))
            (take-count []
              (let [n (parse-int-decimal (take-until 58))]
                #?(:clj (int n) :cljs (js/Number n))))
            (walk []
              (let [tag (take-byte)]
                (case tag
                  110 ["null"]
                  98 (let [b (take-byte)]
                       (cond
                         (= b 116) ["bool" true]
                         (= b 102) ["bool" false]
                         :else (throw (ex-info "document-read invalid bool"
                                               {:phase :value :byte b}))))
                  105 ["i64" (parse-int-decimal (take-until 59))]
                  102 (let [bits (parse-int-decimal (take-until 59))]
                        ["f64" (i64-bits-to-f64 bits)])
                  115 ["string" (take-len-str)]
                  107 (let [kw-str (take-len-str)]
                        (when-not (and (string? kw-str)
                                       (pos? (count kw-str))
                                       (= (first kw-str) \:))
                          (throw (ex-info "document-read keyword must start with colon"
                                          {:phase :value :text kw-str})))
                        ["keyword" (keyword (subs kw-str 1))])
                  121 ["symbol" (symbol (take-len-str))]
                  118 (let [n (take-count)]
                        (when (or (neg? n) (> n document-container-item-limit))
                          (throw (ex-info "document-read vector count out of range"
                                          {:phase :value :count n})))
                        ["vector" (vec (repeatedly n walk))])
                  108 (let [n (take-count)]
                        (when (or (neg? n) (> n document-container-item-limit))
                          (throw (ex-info "document-read list count out of range"
                                          {:phase :value :count n})))
                        ["list" (vec (repeatedly n walk))])
                  101 (let [n (take-count)]
                        (when (or (neg? n) (> n document-container-item-limit))
                          (throw (ex-info "document-read set count out of range"
                                          {:phase :value :count n})))
                        ["set" (vec (repeatedly n walk))])
                  109 (let [n (take-count)]
                        (when (or (neg? n) (> n document-container-item-limit))
                          (throw (ex-info "document-read map count out of range"
                                          {:phase :value :count n})))
                        ["map"
                         (vec
                          (repeatedly n
                            (fn []
                              (let [marker (take-byte)
                                    key (case marker
                                          75 (let [k-str (take-len-str)] ;; 'K'
                                               (when-not (and (string? k-str)
                                                              (pos? (count k-str))
                                                              (= (first k-str) \:))
                                                 (throw (ex-info "document-read map keyword key is invalid"
                                                                 {:phase :value :text k-str})))
                                               ["keyword" (keyword (subs k-str 1))])
                                          68 (walk) ;; 'D'
                                          (throw (ex-info "document-read map entry has invalid key marker"
                                                          {:phase :value :marker marker})))]
                                [key (walk)]))))])
                  (throw (ex-info "document-read unknown tag"
                                  {:phase :value :tag tag})))))]
      (let [doc (walk)]
        (when-not (= @idx len)
          (throw (ex-info "document-read trailing bytes"
                          {:phase :value :offset @idx :length len})))
        doc))))

(defn document-print
  "Deterministic printer for logical documents: lowercase hex of
  document-canonical-bytes. Inverse is document-read. Fails closed when the
  hex encoding would exceed string-value-byte-limit."
  [value]
  (let [hex (bytes->hex (document-canonical-bytes value))]
    (when (> (count hex) string-value-byte-limit)
      (throw (ex-info "document-print exceeds string byte limit"
                      {:phase :value :limit string-value-byte-limit
                       :length (count hex)})))
    hex))

(defn document-read
  "Reader inverse of document-print. Re-validates depth/node/item/byte budgets
  via bounded-document! so host-supplied strings cannot smuggle oversize trees."
  [s]
  (when-not (string? s)
    (throw (ex-info "document-read requires a string" {:phase :value})))
  (when (> (count s) string-value-byte-limit)
    (throw (ex-info "document-read string exceeds byte limit"
                    {:phase :value :limit string-value-byte-limit})))
  (bounded-document! (document-from-canonical-bytes! (hex->bytes! s))))

(defn- document-edn-escape-string [s]
  (bounded-string! s string-value-byte-limit)
  (let [out (volatile! ["\""])]
    (dotimes [index (count s)]
      (let [unit #?(:clj (int (.charAt ^String s index))
                    :cljs (.charCodeAt s index))]
        (vswap! out conj
                (case unit
                  8 "\\b" 9 "\\t" 10 "\\n" 12 "\\f" 13 "\\r"
                  34 "\\\"" 92 "\\\\"
                  (if (< unit 32)
                    (let [hex #?(:clj (Integer/toHexString unit)
                                 :cljs (.toString unit 16))]
                      (str "\\u" (apply str (repeat (- 4 (count hex)) "0")) hex))
                    #?(:clj (str (.charAt ^String s index))
                       :cljs (.charAt s index)))))))
    (apply str (conj @out "\""))))

(defn- document-edn-f64-text [n]
  (when-not (and (f64-value? n)
                 #?(:clj (Double/isFinite ^double n) :cljs (js/Number.isFinite n)))
    (throw (ex-info "document-edn-print rejects non-finite f64" {:phase :value})))
  (let [text #?(:clj (Double/toString ^double n) :cljs (.toString n))]
    ;; EDN must retain a floating marker so integral doubles do not decode as i64.
    (if (or (some #{\. \e \E} text)) text (str text ".0"))))

(defn- document-edn-symbol-text [value]
  (let [text (str (bounded-symbol! value symbol-value-byte-limit))]
    (when (or (contains? #{"nil" "true" "false"} text)
              (.startsWith text ":")
              (.startsWith text "#")
              (re-find #"[\s,\[\]{}()\"';`~^\\]" text)
              (re-matches #"[+-]?[0-9]+" text)
              (re-matches #"[+-]?(?:(?:[0-9]+\.[0-9]*)|(?:[0-9]*\.[0-9]+)|(?:[0-9]+[eE][+-]?[0-9]+))(?:[eE][+-]?[0-9]+)?" text))
      (throw (ex-info "document-edn-print rejects ambiguous symbol"
                      {:phase :value :symbol text})))
    text))

(defn document-edn-print
  "Deterministic textual EDN for the bounded document profile. The profile is
  deliberately closed to nil, booleans, i64/f64, strings, keywords, symbols,
  vectors, lists, sets, and maps with document keys; tagged values and reader eval
  have no document representation."
  [value]
  (letfn [(walk [node]
            (let [[tag payload] node]
              (case tag
                "null" "nil"
                "bool" (if payload "true" "false")
                "i64" (str payload)
                "f64" (document-edn-f64-text payload)
                "string" (document-edn-escape-string payload)
                "keyword" (str payload)
                "symbol" (document-edn-symbol-text payload)
                "vector" (str "[" (str/join " " (map walk payload)) "]")
                "list" (str "(" (str/join " " (map walk payload)) ")")
                "set" (str "#{" (str/join " " (map walk payload)) "}")
                "map" (str "{" (str/join
                                  " " (map (fn [[key item]]
                                             (str (walk key) " " (walk item))) payload)) "}")
                (throw (ex-info "document-edn-print unknown document tag"
                                {:phase :value :tag tag})))))]
    (let [text (walk (bounded-document! value))]
      (bounded-string! text string-value-byte-limit))))

(defn document-edn-read
  "Read one bounded textual EDN form into a document. This is an inert parser:
  tagged and discard dispatch forms are rejected before any host reader or
  resolver can run; the sole admitted dispatch prefix is the bounded set `#{`."
  [text]
  (bounded-string! text string-value-byte-limit)
  (let [cursor (volatile! 0)
        length (count text)]
    (letfn [(fail [message]
              (throw (ex-info (str "document-edn-read " message)
                              {:phase :value :offset @cursor})))
            (at [] (when (< @cursor length)
                     #?(:clj (.charAt ^String text @cursor)
                        :cljs (.charAt text @cursor))))
            (take-char [] (let [ch (at)] (vswap! cursor inc) ch))
            (space? [ch] (contains? #{\space \tab \newline \return \,} ch))
            (delimiter? [ch]
              (or (nil? ch) (space? ch) (contains? #{\[ \] \{ \} \( \) \" \;} ch)))
            (skip []
              (loop []
                (cond
                  (space? (at)) (do (vswap! cursor inc) (recur))
                  (= (at) \;) (do (loop []
                                      (when (and (< @cursor length)
                                                 (not= (take-char) \newline))
                                        (recur)))
                                    (recur)))))
            (hex-unit []
              (when (> (+ @cursor 4) length) (fail "truncated unicode escape"))
              (let [raw (subs text @cursor (+ @cursor 4))]
                (when-not (re-matches #"[0-9A-Fa-f]{4}" raw)
                  (fail "invalid unicode escape"))
                (vswap! cursor + 4)
                #?(:clj (char (Integer/parseInt raw 16))
                   :cljs (js/String.fromCharCode (js/parseInt raw 16)))))
            (read-quoted []
              (take-char)
              (let [parts (volatile! [])]
                (loop []
                  (let [ch (take-char)]
                    (cond
                      (nil? ch) (fail "unterminated string")
                      (= ch \newline) (fail "newline in string")
                      (= ch \return) (fail "newline in string")
                      (= ch \") (apply str @parts)
                      (= ch \\)
                      (let [escaped (take-char)]
                        (vswap! parts conj
                                (case escaped
                                  \b "\b" \t "\t" \n "\n" \f "\f" \r "\r"
                                  \" "\"" \\ "\\" \u (str (hex-unit))
                                  (fail "unsupported string escape")))
                        (recur))
                      :else (do (vswap! parts conj (str ch)) (recur)))))))
            (read-token []
              (let [start @cursor]
                (loop []
                  (when-not (delimiter? (at)) (vswap! cursor inc) (recur)))
                (when (= start @cursor) (fail "expected token"))
                (subs text start @cursor)))
            (parse-i64 [token]
              (try
                (let [n #?(:clj (Long/parseLong token) :cljs (js/BigInt token))]
                  ["i64" n])
                (catch #?(:clj Exception :cljs :default) _ (fail "i64 out of range"))))
            (parse-f64 [token]
              (let [n #?(:clj (try (Double/parseDouble token) (catch Exception _ nil))
                         :cljs (js/Number token))]
                (when-not (and (some? n)
                               #?(:clj (Double/isFinite ^double n)
                                  :cljs (js/Number.isFinite n)))
                  (fail "invalid or non-finite f64"))
                ["f64" n]))
            (parse-token []
              (let [token (read-token)]
                (cond
                  (= token "nil") ["null"]
                  (= token "true") ["bool" true]
                  (= token "false") ["bool" false]
                  (re-matches #"[+-]?[0-9]+" token) (parse-i64 token)
                  (re-matches #"[+-]?(?:(?:[0-9]+\.[0-9]*)|(?:[0-9]*\.[0-9]+)|(?:[0-9]+[eE][+-]?[0-9]+))(?:[eE][+-]?[0-9]+)?" token)
                  (parse-f64 token)
                  (and (.startsWith token ":") (> (count token) 1))
                  ["keyword" (bounded-keyword! (keyword (subs token 1)) keyword-value-byte-limit)]
                  (.startsWith token ":") (fail "invalid keyword")
                  :else ["symbol" (bounded-symbol! (symbol token) symbol-value-byte-limit)])))
            (read-value [depth]
              (when (> depth document-depth-limit) (fail "depth limit exceeded"))
              (skip)
              (case (at)
                nil (fail "unexpected end of input")
                \" ["string" (read-quoted)]
                \[ (do (take-char)
                        (loop [items []]
                          (skip)
                          (if (= (at) \])
                            (do (take-char) ["vector" items])
                            (do (when (>= (count items) document-container-item-limit)
                                  (fail "vector item limit exceeded"))
                                (recur (conj items (read-value (inc depth))))))))
                \( (do (take-char)
                        (loop [items []]
                          (skip)
                          (if (= (at) \))
                            (do (take-char) ["list" items])
                            (do (when (>= (count items) document-container-item-limit)
                                  (fail "list item limit exceeded"))
                                (recur (conj items (read-value (inc depth))))))))
                \{ (do (take-char)
                        (loop [entries []]
                          (skip)
                          (if (= (at) \})
                            (do (take-char)
                                (let [sorted (vec (sort (fn [[left] [right]]
                                                          (document-map-key-compare left right))
                                                        entries))]
                                  (when-not (every? (fn [[[left] [right]]]
                                                      (neg? (document-map-key-compare left right)))
                                                    (partition 2 1 sorted))
                                    (fail "duplicate map key"))
                                  ["map" sorted]))
                            (do (when (>= (count entries) document-container-item-limit)
                                  (fail "map entry limit exceeded"))
                                (let [key-node (read-value (inc depth))]
                                  (skip)
                                  (when (= (at) \}) (fail "map value missing"))
                                  (recur (conj entries [key-node
                                                        (read-value (inc depth))])))))))
                \# (do (take-char)
                        (when-not (= (at) \{) (fail "dispatch forms are forbidden"))
                        (take-char)
                        (loop [items []]
                          (skip)
                          (if (= (at) \})
                            (do (take-char)
                                (let [canonical (vec (sort document-compare items))]
                                  (when-not (every? (fn [[left right]]
                                                      (neg? (document-compare left right)))
                                                    (partition 2 1 canonical))
                                    (fail "duplicate set item"))
                                  ["set" canonical]))
                            (do (when (>= (count items) document-container-item-limit)
                                  (fail "set item limit exceeded"))
                                (recur (conj items (read-value (inc depth))))))))
                \) (fail "unexpected closing delimiter")
                \] (fail "unexpected closing delimiter")
                \} (fail "unexpected closing delimiter")
                (parse-token)))]
      (skip)
      (let [document (read-value 0)]
        (skip)
        (when (< @cursor length) (fail "trailing forms"))
        (bounded-document! document)))))

(def ^:private leaf-value-types
  #{:i64 :f32 :f64 :string :keyword :symbol :map :bool :option-i64 :result-i64
    :vector-i64 :vector-f64 :string-index :disjoint-set-i64 :document :bytes})

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
     (and (vector? type) (= 2 (count type)) (= :stream (first type)))
     (do (when-not (= :bytes (second type))
           (throw (ex-info "stream item type must be :bytes in this profile"
                           {:phase :value :type type})))
         (validate-value-type! (second type) (inc depth) nodes)
         type)
     (and (vector? type) (= 2 (count type)) (= :task (first type)))
     (do (let [inner (second type)]
           (when-not (and (vector? inner) (= :stream (first inner)))
             (throw (ex-info "task result type must be [:stream :bytes] in this profile"
                             {:phase :value :type type})))
           (validate-value-type! inner (inc depth) nodes))
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

(defn- compare-i64
  "Signed numeric order over an i64 VALUE, on both runtimes.

  `compare` cannot do this on `:cljs`. An i64 that came from `.kotoba` source
  is a JS bigint, which is neither `number?` nor `IComparable`, so
  `cljs.core/compare` reaches its final arm and throws `Cannot compare 2 to 1`.
  Every ordered i64 collection went through here, so under nbb none of them
  worked: `[:set :i64]` with two items, and -- once an integer could be a map
  key at all -- `[:map :i64 V]` with two entries. The second one did not look
  like this bug from outside. `amu compile --target wasm32` evaluates the
  oracle through `lower`, so the exception surfaced as a generic `internal
  compiler error` (exit 70) that read as a missing wasm lowering; the entry
  counts (one entry fine, two not) and the key types (only `:i64`) are what
  identify it.

  `<` and `>` are JS operators and do work on bigint -- `kotoba.kir.cljs-i64`
  already relies on that -- so the order is spelled with those. Mixed bigint
  and number operands compare numerically in JavaScript, which is what a
  literal synthesized by the compiler (a plain number) meeting one read from
  source (a bigint) requires."
  [left right]
  #?(:clj (compare left right)
     :cljs (cond (< left right) -1 (> left right) 1 :else 0)))

(defn compare-typed-values
  "Language-owned total order for already validated values of one type."
  [type left right]
  (case type
    :i64 (compare-i64 left right)
    :string (compare left right)
    :bytes (let [n (min (bytes-byte-count left) (bytes-byte-count right))]
             (loop [i 0]
               (if (< i n)
                 (let [a #?(:clj (bit-and 0xff (aget ^bytes left i))
                            :cljs (aget left i))
                       b #?(:clj (bit-and 0xff (aget ^bytes right i))
                            :cljs (aget right i))]
                   (if (= a b)
                     (recur (inc i))
                     (compare a b)))
                 (compare (bytes-byte-count left) (bytes-byte-count right)))))
    :keyword (compare (str left) (str right))
    :symbol (compare (str left) (str right))
    :bool (compare left right)
    :option-i64 (if (= (first left) (first right))
                  (if (first left) (compare-i64 (second left) (second right)) 0)
                  (if (first left) 1 -1))
    :result-i64 (if (= (first left) (first right))
                  (compare-i64 (second left) (second right))
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
     :bytes (let [validated (bounded-bytes! value bytes-value-byte-limit)]
              (charge-indirect! (bytes-byte-count validated))
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

       (= :stream (first type))
       (do
         (when-not (and (= 2 (count type)) (= :bytes (second type)))
           (throw (ex-info "stream type must be [:stream :bytes]" {:phase :value})))
         (when-not (stream-value? value)
           (throw (ex-info "value is not a bytes-stream handle" {:phase :value})))
         (bounded-bytes! (:kotoba.stream/payload value) bytes-value-byte-limit)
         (charge-indirect! (bytes-byte-count (:kotoba.stream/payload value)))
         value)

       (= :task (first type))
       (do
         (when-not (and (= 2 (count type))
                        (= [:stream :bytes] (second type)))
           (throw (ex-info "task type must be [:task [:stream :bytes]]" {:phase :value})))
         (when-not (task-value? value)
           (throw (ex-info "value is not a bytes-task handle" {:phase :value})))
         (when-let [s (:kotoba.task/stream value)]
           (bounded-typed-value! [:stream :bytes] s (inc depth)
                                 nodes indirect-bytes list-items))
         value)

       (schema-ref-type? type)
       (bounded-typed-value! (resolve-schema-ref-type type value) value
                             depth nodes indirect-bytes list-items)

       :else (throw (ex-info "value type is outside the safe profile"
                             {:phase :value})))))))
