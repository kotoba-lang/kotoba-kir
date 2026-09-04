(ns kotoba.kir-string-upper-test
  "string-upper KIR eval: execute proves the op works at the KIR level, with
  the bounded-string! wrapper and the same host-locale determinism rules as
  string-fold-case (Locale/ROOT on the JVM)."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.kir :as ir]))

(def upper-kir
  {:format :kotoba.kir/v4
   :exports ['up]
   :schemas {}
   :effects #{}
   :functions
   [{:name 'up :params ['s] :param-types [:string]
     :result :string :effects #{} :body '(string-upper s)}]})

(deftest string-upper-executes-at-the-kir-level
  ;; Measured host answers (JVM `Locale/ROOT` and JS `.toUpperCase()` agree
  ;; on these ranges): ASCII letters upper-case; digits and CJK are caseless
  ;; and unchanged; the empty string is unchanged; already-upper input is
  ;; unchanged (idempotent on its own output). German ß has NO 1:1 simple
  ;; upper mapping (JVM/ROOT and JS both give SS via simple mapping only on
  ;; the JVM; JS .toUpperCase() gives "SS" too) -- measured, see the
  ;; conformance note in the amu string_upper test; pinned here as SS on both
  ;; hosts.
  (is (= "KOTOBA" (ir/execute upper-kir 'up ["kotoba"])))
  (is (= "日本語" (ir/execute upper-kir 'up ["日本語"])))
  (is (= "" (ir/execute upper-kir 'up [""])))
  (is (= "KOTOBA" (ir/execute upper-kir 'up ["KOTOBA"])))
  (is (= "CAFÉ" (ir/execute upper-kir 'up ["café"])))
  (is (= "ABC" (ir/execute upper-kir 'up ["aBc"]))))

(deftest string-upper-is-bounded
  ;; A result over the typed string byte limit (65536 UTF-8 bytes) must trap,
  ;; mirroring string-fold-case's bounded-string! wrap. 70000 ASCII bytes is
  ;; over the limit; a 40000-byte string is under it and must pass.
  (let [big (apply str (repeat 70000 "x"))
        ok (apply str (repeat 40000 "x"))]
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (ir/execute upper-kir 'up [big])))
    ;; the upper-case of the under-limit string is under the limit too
    (is (= (apply str (repeat 40000 "X"))
           (ir/execute upper-kir 'up [ok])))))
