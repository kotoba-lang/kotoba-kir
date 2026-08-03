(ns kotoba.kir-admission-let-binding-test
  "The two target-admission predicates must see through `let` bindings.

  Both walked a form's `args` generically, and for `let` the first arg is the
  binding VECTOR. A Clojure vector is not `seq?`, so the walk fell straight to
  its terminal case -- `true` in the native predicate, and an explicit
  `(vector? form) true` in the CLJS one, written for sealed type descriptors --
  and the binding VALUES were never inspected.

  The consequence was that the identical operation was gated when written
  directly and admitted when bound by a `let`, which is how `xorshift32`
  reached a native backend while the `u32-wrap` it desugars into was rejected."
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.kir :as kir]))

(defn- hir [body]
  {:format :kotoba.hir/v3
   :functions [{:name 'main :params [] :param-types [] :result :i64 :body body}]})

;; One representative per typed family the native gate is meant to exclude.
;; `string-*` is deliberately absent: the native backend implements a qualified
;; string slice, so those are admitted by design and would prove nothing here.
(def ^:private excluded
  ['(u32-wrap 1)
   '(i32-xor 1 2)
   '(i32-shift-left 1 2)
   '(map-new)
   '(keyword-from-string "k")])

(deftest a-let-binding-does-not-launder-an-excluded-operation
  (doseq [form excluded]
    (testing (str form)
      (is (false? (kir/only-native-word-typed-features? (hir form)))
          "written directly, the operation must be excluded")
      (is (false? (kir/only-native-word-typed-features? (hir (list 'let ['a form] 'a))))
          "binding it to a let must not change that")
      (is (false? (kir/only-native-word-typed-features?
                   (hir (list 'let ['a 1 'b form] 'a))))
          "nor must burying it in a later binding")
      (is (false? (kir/only-native-word-typed-features?
                   (hir (list 'let ['a (list 'let ['b form] 'b)] 'a))))
          "nor nesting the let"))))

(deftest the-cljs-predicate-has-the-same-property
  (doseq [form excluded]
    (testing (str form)
      (is (false? (kir/only-cljs-provider-typed-features? (hir form))))
      (is (false? (kir/only-cljs-provider-typed-features? (hir (list 'let ['a form] 'a))))))))

(deftest ordinary-let-bound-word-values-are-still-admitted
  ;; Tightening the walk must not start rejecting the one-word slice the native
  ;; backends genuinely implement -- otherwise the fix trades a hole for an
  ;; outage.
  (doseq [form ['(let [a 1] a)
                '(let [a 1 b 2] (+ a b))
                '(let [a (+ 1 2)] (* a 3))
                '(let [a true] a)
                '(let [a "s"] (string-byte-length a))
                '(let [a (let [b 2] b)] a)]]
    (testing (str form)
      (is (true? (kir/only-native-word-typed-features? (hir form)))))))

(deftest a-malformed-let-is-rejected-rather-than-walked-past
  ;; An odd binding count or a non-symbol binder cannot come from the frontend,
  ;; but this predicate also runs over KIR the verifier treats as hostile, so
  ;; it must not silently admit a shape it cannot reason about.
  (doseq [form ['(let [a] a)
                '(let [a 1 b] b)
                '(let ["a" 1] 1)
                '(let (a 1) a)]]
    (testing (str form)
      (is (false? (kir/only-native-word-typed-features? (hir form)))))))
