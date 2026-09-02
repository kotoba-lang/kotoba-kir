(ns kotoba.kir-bit-or-typed-admission-test
  "`bit-or` and `bit-not` beside a typed value.

  `only-native-word-typed-features?` runs on `:kotoba.hir/v3` only, and a
  module reaches v3 only by using a typed value. `bit-or` is in
  `non-string-typed-ops` -- it was ADDED to the arithmetic surface after that
  set was written (ADR-2607254600 D2) and nothing gave it an admitting case --
  so the two halves were independently fine and their COMBINATION was refused.

  Nothing saw it because nothing had needed both at once. aiueos
  `qwen35-gguf-kv-scan.kotoba` uses `bit-or` sixty times and no float, so it is
  v2 and never reaches this gate; the f32 objects used no `bit-or`. The first
  object to use both -- a port of `fp16_to_f32`, whose three `|`s are the whole
  of what it does -- came back \"typed values currently require the
  kotoba-script web target, typed Wasm target, or qualified native
  string/scalar-record/option-i64/result-i64 features\", a message about typed
  values that names neither operation.

  Both directions are pinned. The shift families sit in the same set with the
  same absence and are still refused, because they carry an operand restriction
  the two here do not (the count must be an integer literal in range) and this
  increment measured `bit-or` and `bit-not` rather than them."
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.kir :as kir]))

(defn- typed-hir [body]
  ;; v3 is what turns the gate on; the f32 term is what makes it v3.
  {:format :kotoba.hir/v3
   :functions [{:name 'main :params ['a 'b] :param-types [:i64 :i64] :result :i64
                :body (list '+ body '(f32-to-bits (i64-to-f32-rounded a)))}]})

(deftest bit-or-and-bit-not-are-admitted-beside-a-typed-value
  (doseq [body ['(bit-or a b)
                '(bit-not a)
                ;; the shape that found this: sign | exponent | mantissa
                '(bit-or (* (bit-and a 32768) 65536)
                         (bit-or (* (+ b 127) 8388608) (* (bit-and a 1023) 8192)))]]
    (is (kir/only-native-word-typed-features? (typed-hir body))
        (pr-str body))))

(deftest the-integer-word-ops-that-were-already-admitted-stay-admitted
  (doseq [body ['(bit-and a b) '(bit-xor a b) '(quot a b) '(+ a b) '(* a b)]]
    (is (kir/only-native-word-typed-features? (typed-hir body))
        (pr-str body))))

(deftest the-i64-shift-family-is-still-refused
  (testing "not widened here. The refused set is exactly the three i64 shifts
            and xorshift32; their i32 twins ARE admitted, which measures the
            omission as arbitrary rather than principled -- and is a reason to
            widen them deliberately with their own evidence, not as a side
            effect of this one."
    (doseq [body (quote [(i64-shift-left a 3) (i64-shift-right a 3)
                         (u64-shift-right a 3) (xorshift32 a)])]
      (is (not (kir/only-native-word-typed-features? (typed-hir body)))
          (pr-str body))))
  (testing "the i32 twins, measured 2026-09-02"
    (doseq [body (quote [(i32-shift-left a 3) (i32-shift-right a 3)
                         (u32-shift-right a 3) (i32-wrap a) (i32-xor a b)])]
      (is (kir/only-native-word-typed-features? (typed-hir body))
          (pr-str body)))))

(deftest the-format-tag-is-the-callers-gate-not-this-functions
  ;; Why this was invisible for so long: this predicate answers about ANY hir,
  ;; and amu's `compile-native!` only consults it when the module is
  ;; `:kotoba.hir/v3` -- which a module becomes only by using a typed value.
  ;; So `bit-or` compiled on native for as long as nothing beside it was
  ;; typed. The predicate itself refuses the same body under either tag.
  (let [body (quote (i64-shift-left (bit-or a b) 3))]
    (is (not (kir/only-native-word-typed-features?
              (assoc (typed-hir body) :format :kotoba.hir/v2))))
    (is (not (kir/only-native-word-typed-features? (typed-hir body))))))
