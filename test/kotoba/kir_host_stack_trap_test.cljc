(ns kotoba.kir-host-stack-trap-test
  "Unbounded non-helper recursion aborts `lower` with a Kotoba trap, on BOTH
  runtimes.

  `lower`'s own comment about the constant oracle says such a program \"still
  traps (fuel or host-stack) and aborts lower -- intentional\". Until
  2026-08-24 that was true on the JVM only: the `StackOverflowError` catch was
  inside `#?(:clj ...)` with nothing on the `:cljs` side, so ClojureScript let
  a raw `RangeError: Maximum call stack size exceeded` out of the compiler.

  WHICH limit is reached first is a property of the host, not of the program,
  and that is the whole reason this went unseen: on nbb 1.4.210 amu's
  `lazy-sequence` fixture lowers fine and on nbb 1.5.212 it raises. Every test
  in this repository was `.clj`, and the fleet gate is `:jvm-test`, so the
  `:cljs` branch had never been executed at all.

  So this asserts the INVARIANT and not the limit: whatever runs out first,
  what comes out is `:fuel-exhausted` with `:phase :ir`, and never a host
  error object."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing] :include-macros true])
            [kotoba.kir :as ir]
            [kotoba.test-hir :as test-hir]))

(defn- unbounded-recursion-hir
  "`(defn f [n] (if (<= n 0) 0 (+ 1 (f (- n 1)))))` called with `n`.

  NOT a self-tail call: the recursive call sits under `+`, so the loop-helper
  trampoline cannot flatten it and the interpreter really does nest. That is
  the point -- `kir_oracle_fuel_test` already covers the trampolined shape,
  which is exactly why it never reached this path."
  [n]
  (test-hir/module
   {:format :kotoba.hir/v3
    :entry 'main
    :exports ['main]
    :result :i64
    :schemas {}
    :schema-identities {}
    :functions
    [{:name 'main :params [] :param-types [] :result :i64
      :body (list 'deep n)}
     {:name 'deep :params ['n] :param-types [:i64] :result :i64
      :body (list 'if (list '<= 'n 0)
                  0
                  (list '+ 1 (list 'deep (list '- 'n 1))))}]}))

(defn- trap-data
  "The trap `e` carries, looked for down the whole cause chain.

   Reading `(ex-data e)` one link deep is not enough on ClojureScript: SCI
   wraps a throw that crosses its own boundary in an error whose ex-data is
   `{:type :sci/error :line .. :column ..}` -- non-nil, and WITHOUT `:trap`.
   A classifier that stops at the first link therefore sees `:trap nil` and
   reports a Kotoba trap as an unclassified failure. Measured here at depth 16
   on nbb 1.4.210: the same program reports `:trap :fuel-exhausted` when
   `lower` is called directly and `:trap nil` when it is called from inside a
   `deftest`, because the wrapping depends on what is between the two.

   This is the defect scripts/fleet-ci/gates.edn already records against
   kotobase-server (root ADR-2608200100): `nbb/SCI wraps a throw that crosses
   an async continuation, so classifiers reading (ex-data e) one link deep
   lost the marker that selects the outcome`."
  [e]
  (loop [x e]
    (when x
      (let [data (ex-data x)]
        (if (:trap data)
          data
          (recur #?(:clj (.getCause ^Throwable x) :cljs (.-cause x))))))))

(defn- lower-outcome
  "`:folded`, or the trap that aborted `lower`, or -- if there is no trap
   anywhere in the chain -- the host error that escaped."
  [n]
  (try (do (ir/lower (unbounded-recursion-hir n)) :folded)
       (catch #?(:clj Throwable :cljs :default) e
         (or (trap-data e)
             {:host-error-escaped (str (ex-message e))}))))

(deftest unbounded-recursion-aborts-lower-with-a-kotoba-trap
  (testing "the compiler refuses, and refuses in its own vocabulary"
    (let [outcome (lower-outcome 1000000)]
      (is (not= :folded outcome)
          "a program the oracle cannot finish must abort lower, not return")
      (is (nil? (:host-error-escaped outcome))
          (str "a host error escaped the language boundary: "
               (pr-str (:host-error-escaped outcome))))
      (is (= :ir (:phase outcome)))
      (is (= :fuel-exhausted (:trap outcome))
          "fuel or host stack, the trap the caller sees is the same one"))))

(deftest a-shallow-program-still-folds
  (testing "the guard did not turn a working oracle into a trap"
    ;; Depth 3, because the oracle's real ceiling is a property of the HOST and
    ;; not of the compiler -- and it is not even a stable property of one host.
    ;; Measured 2026-08-24 on the same source, called directly: nbb 1.4.210
    ;; folds 12 and traps at 16; nbb 1.5.212 folds 4 and traps at 8. Called
    ;; from inside `deftest` both ceilings drop again, because cljs.test's own
    ;; frames are already on the stack. Each interpreter spends a different
    ;; amount of JS stack per interpreted call, and so does each caller.
    ;;
    ;; Writing any of those numbers here would pin the test to one runtime on
    ;; one call path. 3 is under every ceiling seen, and
    ;; `crossing-the-ceiling-is-a-trap-not-a-host-error` below covers the
    ;; interesting part without naming a depth at all.
    ;;
    ;; `str` rather than `=`: the oracle hands back a host i64, which is a Long
    ;; on the JVM and a BigInt on ClojureScript, and `(= 3 #object[BigInt 3])`
    ;; is false.
    (is (= "3" (str (:oracle-value (ir/lower (unbounded-recursion-hir 3))))))))

(deftest crossing-the-ceiling-is-a-trap-not-a-host-error
  (testing "whatever this host's limit is, going past it stays inside the language"
    ;; Self-calibrating on purpose. The ceiling moved 12 -> 4 between two nbb
    ;; releases and moves again with the caller's own stack, so a test that
    ;; names a depth tests the release.
    ;;
    ;; The outcome is KEPT from the probing call, not re-measured at the depth
    ;; the probe returned. The first version of this test did re-measure, and
    ;; failed: on nbb 1.4.210 depth 16 aborted while searching and FOLDED when
    ;; called again a moment later, because the two calls do not start from the
    ;; same stack. `(:trap :folded)` is nil, so the failure read as `no trap`
    ;; when what actually happened was `no abort`. The ceiling is not a stable
    ;; property of a host -- it is a property of a call.
    (let [[depth outcome]
          (first (keep (fn [n]
                         (let [o (lower-outcome n)]
                           (when (not= :folded o) [n o])))
                       (take 24 (iterate #(* 2 %) 1))))]
      (is (some? depth)
          "no depth up to 2^23 aborted lower -- the oracle cannot be unbounded")
      (is (nil? (:host-error-escaped outcome))
          (str "at depth " depth " a host error escaped: "
               (pr-str (:host-error-escaped outcome))))
      (is (= :fuel-exhausted (:trap outcome))
          (str "at depth " depth)))))
