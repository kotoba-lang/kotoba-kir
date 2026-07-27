(ns kotoba.kir-test
  (:require [clojure.test :refer [deftest is]]
            [kotoba.kir]
            [kotoba.kir.value]
            [kotoba.kir.target :as target]
            [kotoba.kir.decimal]
            [kotoba.kir.xml]
            [kotoba.kir.compatibility]
            [kotoba.kir.admission]))

;; Load gate: the split must not break namespace resolution. Each extracted
;; namespace must load standalone from this repo's own dependency closure.
(deftest every-extracted-namespace-loads
  (is (some? (find-ns 'kotoba.kir)) "kotoba.kir must load")
  (is (some? (find-ns 'kotoba.kir.value)) "kotoba.kir.value must load")
  (is (some? (find-ns 'kotoba.kir.target)) "kotoba.kir.target must load")
  (is (some? (find-ns 'kotoba.kir.decimal)) "kotoba.kir.decimal must load")
  (is (some? (find-ns 'kotoba.kir.xml)) "kotoba.kir.xml must load")
  (is (some? (find-ns 'kotoba.kir.compatibility)) "kotoba.kir.compatibility must load")
  (is (some? (find-ns 'kotoba.kir.admission)) "kotoba.kir.admission must load"))

(deftest typed-component-target-is-a-non-ambient-component
  (is (= {:execution :component
          :abi :component-canonical-abi-v2
          :runtime :kotoba-component-runtime-v2
          :wasi-version "0.3.0"
          :ambient-wasi false}
         (select-keys (target/profile :wasm-component-kotoba-v2)
                      [:execution :abi :runtime :wasi-version :ambient-wasi]))))
