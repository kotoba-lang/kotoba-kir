(ns kotoba.kir.descriptor
  "Structural type descriptors over KIR, and their canonical byte encoding.

  Extracted from `kotoba.wasm.typed` (2026-08-04, ADR-2608049000) without
  behaviour change. Nothing here was ever wasm-specific: the encoding walks
  KIR to find which types a module uses and serialises them, and its only
  dependency was `kotoba.kir.cljs-i64` for BigInt-safe map keys. It lived in
  the wasm repo because that is where it was first needed, which is a fact
  about extraction order rather than about coupling.

  It moves to T0 because a SECOND consumer needs it. The native capability
  boundary can carry exactly four shapes today -- i64, string, option-i64,
  result-i64 -- because the loader is handed an integer `kind` rather than a
  structure, so a capability whose request is a variant with a record payload
  (`clock-v1` is one) cannot cross it at all. Fixing that means giving the
  loader a descriptor, and there must be exactly ONE encoding of a descriptor:
  two would look correct on both sides while disagreeing, which is the failure
  mode where a producer and a checker agree on the same mistake.

  `kotoba.wasm.typed` re-exports these names, so wasm callers are unchanged.

  No require: measured after extraction, the descriptor half never touched
  `kotoba.kir.cljs-i64`. Its single use is in `infer-type`, which stays with
  the wasm-specific half.")

(def primitive-tags
  {:i64 0 :string 1 :keyword 2 :bool 3 :symbol 19 :vector-i64 11 :f64 12 :f32 13
   :vector-f64 14 :string-index 16 :disjoint-set-i64 17 :document 18 :bytes 21})

(def scalar-adt-aliases
  {:option-i64 [:option :i64]
   :result-i64 [:result :i64 :i64]})

(def boolean-result-ops
  '#{f64-eq f64-lt f64-le f64-gt f64-ge f64-unordered
     f32-eq f32-lt f32-le f32-gt f32-ge f32-unordered
     bool-not option-some? result-ok? option-some?-of result-ok?-of
     typed-set-contains typed-map-contains document-contains document-equal?})


(defn descriptor? [value]
  ;; nbb cannot hash JavaScript BigInt as a map key. Guard before contains?
  ;; because literal i64 values are walked alongside descriptors.
  (or (and (keyword? value)
           (or (contains? primitive-tags value)
               (contains? scalar-adt-aliases value)))
      (and (vector? value)
           (or (= :list (first value))
               (contains? #{:option :result :variant :vector :set :map :record :ref}
                          (first value))))))

(defn uleb [n]
  (loop [n n out []]
    (let [byte (bit-and n 0x7f)
          remaining (unsigned-bit-shift-right n 7)]
      (if (zero? remaining)
        (conj out byte)
        (recur remaining (conj out (bit-or byte 0x80)))))))

(defn utf8 [text]
  #?(:clj (mapv #(bit-and (int %) 0xff) (.getBytes ^String text "UTF-8"))
     :cljs (vec (js/Array.from (.encode (js/TextEncoder.) text)))))

(defn text-bytes [text]
  (let [bytes (utf8 text)]
    (into (uleb (count bytes)) bytes)))

(defn keyword-text [value]
  (str value))

(declare encode-descriptor)

(defn- encode-named-members [members]
  (into (uleb (count members))
        (mapcat (fn [[member-name member-type]]
                  (concat (text-bytes (keyword-text member-name))
                          (encode-descriptor member-type)))
                members)))

(defn encode-descriptor [descriptor]
  (if-let [expanded (get scalar-adt-aliases descriptor)]
    (encode-descriptor expanded)
    (if-let [tag (get primitive-tags descriptor)]
    [tag]
    (case (first descriptor)
      :option (into [4] (encode-descriptor (second descriptor)))
      :result (into [5] (concat (encode-descriptor (second descriptor))
                                (encode-descriptor (nth descriptor 2))))
      :variant (into [6] (concat (text-bytes (keyword-text (second descriptor)))
                                 (encode-named-members (nth descriptor 2))))
      :vector (into [7] (concat (uleb (count (second descriptor)))
                                (mapcat encode-descriptor (second descriptor))))
      :set (into [8] (encode-descriptor (second descriptor)))
      :list (into [20] (encode-descriptor (second descriptor)))
      :map (into [10] (concat (encode-descriptor (second descriptor))
                              (encode-descriptor (nth descriptor 2))))
      :record (into [9] (concat (text-bytes (keyword-text (second descriptor)))
                                (encode-named-members (nth descriptor 2))))
      :ref (into [15] (text-bytes (keyword-text (second descriptor))))
      (throw (ex-info "unsupported typed descriptor"
                      {:phase :kir-descriptor :descriptor descriptor}))))))

(defn- walk [value found]
  (cond
    (descriptor? value)
    (let [found (conj found value)]
      (if-not (vector? value)
        found
        (case (first value)
          :option (walk (second value) found)
          :result (->> found (walk (second value)) (walk (nth value 2)))
          :variant (reduce (fn [result [_ type]] (walk type result)) found (nth value 2))
          :vector (reduce (fn [result type] (walk type result)) found (second value))
          :set (walk (second value) found)
          :list (walk (second value) found)
          :map (->> found (walk (second value)) (walk (nth value 2)))
          :record (reduce (fn [result [_ type]] (walk type result)) found (nth value 2))
          :ref found
          found)))

    (and (seq? value) (contains? boolean-result-ops (first value)))
    (reduce (fn [result item] (walk item result))
            (conj found :bool)
            value)
    (and (seq? value)
         (contains? '#{option-some option-none option-some? option-value}
                    (first value)))
    (reduce (fn [result item] (walk item result))
            (conj found :option-i64)
            value)
    (and (seq? value)
         (contains? '#{result-ok result-err result-ok? result-value result-error}
                    (first value)))
    (reduce (fn [result item] (walk item result))
            (conj found :result-i64)
            value)
    ;; T4.5 / T1.3: intermediate vector-i64 (e.g. pure main returning i64 from
    ;; vector-count/at) must seal :vector-i64 even when it never appears in an
    ;; export signature — mirror the vector-f64 body walk.
    (and (seq? value)
         (contains? '#{vector-new vector-count vector-get vector-at
                      vector-drop vector-assoc vector-conj}
                    (first value)))
    (reduce (fn [result item] (walk item result))
            (conj found :vector-i64)
            value)
    (and (seq? value)
         (contains? '#{vector-f64-new vector-f64-count vector-f64-get vector-f64-at
                      vector-f64-drop vector-f64-assoc vector-f64-conj}
                    (first value)))
    (reduce (fn [result item] (walk item result))
            (conj found :vector-f64)
            value)
    (and (seq? value)
         (contains? '#{string-index-new string-index-count string-index-contains
                      string-index-get string-index-assoc}
                    (first value)))
    (reduce (fn [result item] (walk item result)) (conj found :string-index) value)
    (and (seq? value)
         (contains? '#{disjoint-set-i64-new disjoint-set-i64-count disjoint-set-i64-union}
                    (first value)))
    (reduce (fn [result item] (walk item result)) (conj found :disjoint-set-i64) value)
    (and (seq? value)
         (contains? '#{document-null document-bool document-i64 document-f64
                      document-string document-keyword document-symbol document-vector document-list document-set document-map
                      document-count document-kind document-vector-at document-list-at document-map-entry-at document-vector-assoc
                      document-vector-conj document-vector-drop document-vector-remove
                      document-equal? document-set-contains? document-sha256 document-print document-read
                      document-edn-print document-edn-read document-contains document-get document-assoc
                      document-dissoc document-merge document-string-value document-keyword-value document-symbol-value
                      document-bool-value document-i64-value document-f64-value}
                    (first value)))
    (reduce (fn [result item] (walk item result))
            (cond-> (conj found :document)
              (= 'document-kind (first value)) (conj :keyword)
              (= 'document-sha256 (first value)) (conj :string)
              (contains? '#{document-print document-edn-print} (first value)) (conj :string))
            value)
    (and (seq? value) (= 'keyword-from-string (first value)))
    (reduce (fn [result item] (walk item result)) (conj found :keyword) value)
    (and (seq? value) (= 'symbol (first value)))
    (reduce (fn [result item] (walk item result)) (conj found :symbol) value)
    (and (seq? value) (= 'bytes-empty (first value)))
    (conj found :bytes)
    (map? value) (reduce (fn [result item] (walk item result)) found (vals value))
    (coll? value) (reduce (fn [result item] (walk item result)) found value)
    (string? value) (conj found :string)
    (keyword? value) (conj found :keyword)
    (boolean? value) (conj found :bool)
    :else found))

(defn descriptor-table [kir]
  (->> (walk kir #{})
       (sort-by pr-str)
       vec))

(defn descriptor-indices [kir]
  (into {} (map-indexed (fn [index descriptor] [descriptor index])
                        (descriptor-table kir))))

(defn capability-contracts
  "Returns the sealed typed capability contracts used by KIR. One capability
  id has exactly one request/result contract per module."
  [kir]
  (let [contracts (->> (:functions kir)
                       (mapcat #(tree-seq coll? seq (:body %)))
                       (keep (fn [form]
                               (when (and (seq? form) (= 'typed-cap-call (first form)))
                                 (let [[_ id request-type result-type] form]
                                   {:id id :request-type request-type :result-type result-type}))))
                       distinct
                       (sort-by (juxt :id (comp pr-str :request-type) (comp pr-str :result-type)))
                       vec)]
    (doseq [[id grouped] (group-by :id contracts)]
      (when (> (count grouped) 1)
        (throw (ex-info "typed capability id has conflicting contracts"
                        {:phase :kir-descriptor :capability id :contracts grouped}))))
    contracts))
