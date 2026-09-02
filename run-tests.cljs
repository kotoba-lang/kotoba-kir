;; nbb --classpath "src:test:$(clojure -Spath -M:test)" run-tests.cljs
;;
;; The portable side of this suite. Not most of it: kotoba.kir is `.cljc` and
;; claims two runtimes, but every test here was `.clj` and the fleet gate is
;; `:jvm-test`, so until 2026-08-24 the `:cljs` branches of kir.cljc had never
;; been executed by anything. One of them was missing a guard the `:clj`
;; branch had, and a raw host `RangeError` came out of `lower` because of it.
;;
;; Anything added to `test/` as `.cljc` belongs in BOTH lists below -- being
;; required is not being run. `scripts/verify-cljs-runner-completeness.cljs` in
;; the superproject measures this file against the directory.
(ns run-tests
  (:require [cljs.test :as t]
            [kotoba.kir-cljs-i64-ashr-test]
            ;; a core let takes one body form; a core if takes three parts
            [kotoba.kir-core-form-shape-test]
            [kotoba.kir-document-container-index-test]
            [kotoba.kir-document-sha256-test]
            [kotoba.kir-host-stack-trap-test]
            ;; boot-scratch: the two heads that name a place in the image
            [kotoba.kir-image-address-test]
            [kotoba.kir-kernel-memory-test]
            [kotoba.kir-kernel-privileged-test]
            [kotoba.kir-loop-helper-tail-position-test]
            [kotoba.kir-rodata-literal-test]
            ;; slice-value: the ADR 0285 carrier's semantics and its named refusal
            [kotoba.kir-slice-carrier-test]
            [kotoba.kir-uleb-i64-test]))

(defmethod t/report [:cljs.test/default :end-run-tests] [m]
  (println (str "\nnbb: " (:test m) " tests, " (:pass m) " passed, "
                (:fail m) " failed, " (:error m) " errors"))
  (when (pos? (+ (or (:fail m) 0) (or (:error m) 0)))
    (set! (.-exitCode js/process) 1)))

(t/run-tests 'kotoba.kir-cljs-i64-ashr-test
             'kotoba.kir-core-form-shape-test
             'kotoba.kir-document-container-index-test
             'kotoba.kir-document-sha256-test
             'kotoba.kir-host-stack-trap-test
             'kotoba.kir-image-address-test
             'kotoba.kir-kernel-memory-test
             'kotoba.kir-kernel-privileged-test
             'kotoba.kir-loop-helper-tail-position-test
             'kotoba.kir-rodata-literal-test
             'kotoba.kir-slice-carrier-test
             'kotoba.kir-uleb-i64-test)
