(ns kotoba.kir-string-index-of-test
  "String search surface (kbb scripts-port wave 2): KIR eval of
  `string-index-of` -- first UTF-8 BYTE offset, -1 absent, empty needle traps
  (`lang/guest-grammar.edn`: \"Clojure's .indexOf contract, byte-unit\").

  Two things this suite pins that its `.clj` predecessor could not:

  1. The constant oracle. `lower` folds a pure `:i64` entry through the
     interpreter, so a program whose `main` is `(string-index-of ...)` must
     LOWER, not only `execute`. When the op case was absent the oracle fell
     through to `invoke-function` and the compile died with
     `:unknown-function` -- for the pure program only; an effectful one was
     never folded and compiled fine.

  2. The ClojureScript host. kir.cljc claims two runtimes, and every i64 the
     `:cljs` branch produces is a BigInt (`i64/->bigint`), but this op's case
     returned the host's plain JS number. Until this file became `.cljc` and
     was listed in `run-tests.cljs`, that branch had never been executed."
  (:require #?(:clj  [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer [deftest is testing] :include-macros true])
            [kotoba.kir :as ir]
            [kotoba.test-hir :as test-hir]
            #?(:cljs [kotoba.kir.cljs-i64 :as i64])))

(defn- module
  "A pure zero-arity `main` whose body is BODY, as `lower` receives it."
  [body]
  (test-hir/module
   {:format :kotoba.hir/v3
    :entry 'main
    :exports '[main]
    :result :i64
    :schemas {}
    :schema-identities {}
    :functions [{:name 'main :params [] :param-types [] :result :i64
                 :body body}]}))

(defn- run
  "Execute BODY at the KIR level (no oracle)."
  [body]
  (ir/execute (ir/lower (module body)) 'main []))

(defn- fold
  "The constant oracle's answer for BODY."
  [body]
  (:oracle-value (ir/lower (module body))))

(defn- refusal [thunk]
  (try (do (thunk) nil)
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
         (merge {:message (ex-message e)} (ex-data e)))))

(defn- w [x] #?(:clj x :cljs (js/Number x)))

(deftest index-of-basics
  (is (= 3 (w (run '(string-index-of "abcdef" "de")))))
  (is (= -1 (w (run '(string-index-of "abcdef" "xyz")))))
  (is (= 0 (w (run '(string-index-of "abc" "abc")))))
  (is (= 1 (w (run '(string-index-of "abcbc" "bc"))))))

(deftest index-of-utf8-byte-offsets
  ;; "あ" is 3 UTF-8 bytes; "i" therefore sits at byte offset 3 in "あi".
  (is (= 3 (w (run '(string-index-of "あi" "i")))))
  ;; The needle itself is multi-byte and must match by bytes.
  (is (= 0 (w (run '(string-index-of "日本語" "日本")))))
  (is (= 3 (w (run '(string-index-of "日本語" "本")))))
  ;; "héllo " is h(1) é(2) l l o space = 7 bytes, so "wö" starts at byte 7
  ;; -- the same haystack/needle the JS emitter and the wasm typed host are
  ;; measured against. A UTF-16 answer would say 6.
  (is (= 7 (w (run '(string-index-of "héllo wörld" "wö"))))))

(deftest index-of-lowers-through-the-constant-oracle
  ;; The gap this file closes: a PURE entry is folded by `lower`, and the
  ;; oracle must have a case for the op or the compile fails.
  (is (= 7 (w (fold '(string-index-of "héllo wörld" "wö")))))
  (is (= -1 (w (fold '(string-index-of "abc" "zz")))))
  ;; ...and the folded value is a genuine i64 in the surrounding arithmetic
  ;; and comparison, on both hosts.
  (is (= 8 (w (fold '(+ (string-index-of "héllo wörld" "wö") 1)))))
  (is (= 1 (w (fold '(if (= (string-index-of "héllo wörld" "wö") 7) 1 0))))))

#?(:cljs
   (deftest index-of-answers-a-bigint-on-clojurescript
     ;; Every other i64-producing string op (`string-length`,
     ;; `string-byte-length`, `string-code-point-at`, `string-split-count`)
     ;; hands back `i64/->bigint`; the oracle value of this one must be the
     ;; same kind of thing or a consumer comparing oracle values across ops
     ;; sees a number next to a BigInt.
     (is (i64/bigint-value? (fold '(string-index-of "héllo wörld" "wö"))))
     (is (i64/bigint-value? (fold '(string-index-of "abc" "zz"))))))

(deftest empty-needle-traps
  (let [r (refusal #(run '(string-index-of "abc" "")))]
    (is (some? r) "expected trap")
    (is (= :empty-string-search-needle (:trap r)))))

(deftest sibling-search-ops-agree-with-the-contract
  (testing "string-split-count: segment count, non-overlapping, non-empty sep"
    (is (= 3 (w (fold '(string-split-count "a,b,c" ",")))))
    (is (= 1 (w (fold '(string-split-count "" ",")))))
    (is (= 3 (w (fold '(string-split-count "x--y--z" "--")))))
    ;; multi-byte separator, counted in segments not bytes
    (is (= 2 (w (fold '(string-split-count "日本語" "本")))))
    (is (= :empty-string-split-separator
           (:trap (refusal #(run '(string-split-count "abc" "")))))))
  (testing "string-contains?: bool, same empty-needle trap"
    (is (= 1 (w (fold '(if (string-contains? "héllo wörld" "wö") 1 0)))))
    (is (= 0 (w (fold '(if (string-contains? "héllo wörld" "xyz") 1 0)))))
    (is (= :empty-string-search-needle
           (:trap (refusal #(run '(if (string-contains? "abc" "") 1 0))))))))
