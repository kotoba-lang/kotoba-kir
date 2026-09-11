(ns kotoba.kir-rodata-literal-test
  "boot-lit: read-only literals and the two wider firmware calls, at the oracle.

  The three ADDRESS heads refuse, for a reason that is not the privileged
  family's: this interpreter has no image, no load base and no literal pool,
  so any number it returned would be a number the caller hands to firmware as
  a `CHAR16 *`. `bytes-literal-length` does NOT refuse, and that asymmetry is
  the decision this file exists to hold still -- the length is a property of
  the literal text and refusing something answerable buys nothing."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing] :include-macros true])
            [kotoba.kir :as kir]))

(defn- module [params body]
  {:format :kotoba.kir/v4
   :entry 'main
   :effects #{}
   :functions [{:name 'main :params params
                :param-types (vec (repeat (count params) :i64))
                :result :i64 :effects #{} :body body}]})

(defn- trapped [thunk]
  (try (do (thunk) nil)
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e (ex-data e))))

(def ^:private address-cases
  {'ucs2          (module '[] '(ucs2 "AIUEOS"))
   'guid          (module '[] '(guid "5B1B31A1-9562-11D2-8E3F-00A0C969723B"))
   'bytes-literal (module '[] '(bytes-literal "48656c6c6f"))})

(deftest a-literals-address-refuses-at-the-oracle
  (doseq [[op m] address-cases]
    (testing (str op " traps as :rodata-address-unavailable")
      (let [data (trapped #(kir/execute m 'main [] {}))]
        (is (= :rodata-address-unavailable (:trap data)) op)
        (is (= op (:operation data)) op))))
  (is (= 3 (count address-cases))))

(deftest the-refusal-is-its-own-and-not-the-privileged-one
  ;; A privileged operation names an instruction this machine is not running.
  ;; A literal address names a place in an image that does not exist. Sharing
  ;; one keyword would make the two indistinguishable to a caller deciding
  ;; whether the program is wrong or the oracle simply cannot answer.
  (is (= :kernel-privileged-unavailable
         (:trap (trapped #(kir/execute (module '[] '(kernel-system-table))
                                       'main [] {})))))
  (is (= :rodata-address-unavailable
         (:trap (trapped #(kir/execute (module '[] '(ucs2 "x")) 'main [] {}))))))

(defn- i64 [n]
  ;; An i64 is a `long` on the JVM and a BigInt on ClojureScript, and
  ;; `(= 5 (js/BigInt 5))` is false. Comparing against the host's own i64
  ;; representation is what makes this assertion mean the same thing on both
  ;; runtimes rather than only being written once.
  #?(:clj (long n) :cljs (js/BigInt n)))

(deftest a-literals-length-is-answered-because-it-has-an-answer
  (is (= (i64 5) (kir/execute (module '[] '(bytes-literal-length "48656c6c6f"))
                             'main [] {})))
  (is (= (i64 0) (kir/execute (module '[] '(bytes-literal-length "")) 'main [] {})))
  (testing "and it is the byte count, not the digit count"
    (is (= (i64 1) (kir/execute (module '[] '(bytes-literal-length "ff")) 'main [] {})))))

(deftest a-malformed-hex-literal-traps-rather-than-rounding
  (doseq [[label text] [["an odd digit count" "abc"]
                        ["a non-hex digit" "0z"]]]
    (testing label
      (let [data (trapped #(kir/execute
                            (module '[] (list 'bytes-literal-length text))
                            'main [] {}))]
        (is (= :rodata-literal-malformed (:trap data)) label)))))

(deftest the-two-wider-firmware-calls-refuse-like-the-narrow-one
  (doseq [[op m arguments]
          [['kernel-uefi-call4
            (module '[b s a0 a1 a2 a3] '(kernel-uefi-call4 b s a0 a1 a2 a3))
            [4096 8 1 2 3 4]]
           ['kernel-uefi-call6
            (module '[b s a0 a1 a2 a3 a4 a5]
                    '(kernel-uefi-call6 b s a0 a1 a2 a3 a4 a5))
            [4096 8 1 2 3 4 5 6]]]]
    (testing (str op)
      (let [data (trapped #(kir/execute m 'main arguments {}))]
        (is (= :kernel-privileged-unavailable (:trap data)) op)
        (is (= op (:operation data)) op)))))
