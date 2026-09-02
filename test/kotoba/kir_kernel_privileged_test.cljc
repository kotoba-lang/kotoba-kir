(ns kotoba.kir-kernel-privileged-test
  "What this interpreter must REFUSE to answer, and what `lower` must therefore
  never start folding.

  Every operation asserted here names machine or kernel state that this process
  is not running on. The interpreter cannot invent a value for one, and the
  danger is not that it would invent a wrong number -- it is that every real
  call site BRANCHES on the number. `kernel-cpuid-edx` answered at compile time
  is \"this CPU supports NX\" decided by a compiler that has never seen the CPU.

  Two separate sets carry that, and missing either one is a different failure:

  - `eval-expr` must trap, so no oracle can succeed with an invention;
  - `lower`'s `kernel-operations` must contain the operation, so the constant
    oracle is never started at all. Every real call site passes literals (leaf
    `0x80000001` subleaf `0`; XCR index `0`), so a folder has every structural
    reason to try. The trap makes that failure loud rather than silent -- but
    loud is still wrong: it aborts the compile of a valid program.

  `kernel-xgetbv` is the operation this namespace was added for. The `cpuid`,
  MSR and port rows beside it are not decoration: they pin the class it joins,
  so a change that quietly starts ANSWERING one of them fails here rather than
  in a kernel."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.kir :as kir]
            [kotoba.test-hir :as test-hir]))

(defn- module [body]
  (test-hir/module
   {:format :kotoba.hir/v3 :entry 'main :exports ['main] :result :i64
    :functions [{:name 'main :params [] :param-types [] :result :i64
                 :body body}]}))

;; The i64 an oracle seals is a host BigInt on ClojureScript and a long on the
;; JVM -- the same convention `kir_kernel_memory_test` already carries. Only
;; the folded control below compares a number at all; the rows that matter
;; compare `nil`, which needs no narrowing.
(defn- w [x] #?(:clj x :cljs (js/Number x)))

(defn- trapped [thunk]
  (try (thunk) nil
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e (ex-data e))))

;; ---------------------------------------------------------------------------
;; The interpreter refuses
;; ---------------------------------------------------------------------------

;; `xgetbv` reads XCR[ecx] into edx:eax, so `kernel-xgetbv` takes exactly the
;; XCR index and returns the two halves as one i64. Arity is the whole of what
;; the frontend verifies for a privileged operation, and it is stated in three
;; repositories -- `kotoba-sema`'s `kernel-privileged-operations`,
;; `kotoba-gmir`'s `x86-privileged-action-arities`, and `kotoba-native`'s
;; emitter -- with no mechanism keeping them equal but review.
;;
;; NOTE for anyone extending this: `kernel-cpuid-*` ALREADY take (leaf,
;; subleaf). All four are arity 2. A `kernel-cpuid-subleaf-*` family would be a
;; duplicate; leaf 7 subleaf 0 is spelled `(kernel-cpuid-ebx 7 0)`.
(def ^:private refusals
  ['(kernel-xgetbv 0)
   '(kernel-cpuid-eax 1 0)
   '(kernel-cpuid-ebx 7 0)
   '(kernel-cpuid-ecx 1 0)
   '(kernel-cpuid-edx 2147483649 0)
   '(kernel-read-msr 192)
   '(kernel-in-u8 1016)])

(deftest machine-state-is-not-invented
  (doseq [body refusals]
    (let [data (trapped #(kir/execute (kir/lower (module body)) 'main []
                                      {:fuel 10000}))]
      (is (= :kernel-privileged-unavailable (:trap data))
          (str body " must refuse rather than answer"))
      (is (= (first body) (:operation data))
          (str body " must name itself in the trap"))))
  ;; An empty table is not a green suite.
  (is (= 7 (count refusals)) "SCANNED refusals"))

;; ---------------------------------------------------------------------------
;; `lower` does not start an oracle it cannot finish
;; ---------------------------------------------------------------------------

;; This is the assertion that fails if `kernel-xgetbv` is absent from `lower`'s
;; `kernel-operations`. The module has no parameters, no effects and an `:i64`
;; result, which is exactly the shape `lower` folds -- so without the
;; membership it calls `execute`, the trap above fires, and `lower` itself
;; throws.
(deftest xgetbv-marks-a-module-kernel-native
  (let [lowered (kir/lower (module '(kernel-xgetbv 0)))]
    (is (nil? (:oracle-value lowered))
        "an XCR0 read has no compile-time value")
    (is (= [] (:blocks lowered))
        "and therefore no folded constant block"))
  ;; The control. Without this row, a `lower` that had stopped folding
  ;; EVERYTHING would pass the assertions above.
  (is (= 7 (w (:oracle-value (kir/lower (module '(bit-or 6 1))))))
      "a pure expression over literals still folds")
  ;; And the mixed case, which is the one that actually reaches a kernel: one
  ;; XCR0 read anywhere in the module suppresses folding for the module. The
  ;; `(bit-and ... 6)` around it is exactly the AVX2 guard's XCR0 test.
  (is (nil? (:oracle-value (kir/lower (module '(bit-and (kernel-xgetbv 0) 6)))))
      "one privileged read anywhere suppresses the module's oracle"))
