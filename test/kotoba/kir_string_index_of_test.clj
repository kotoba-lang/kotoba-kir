(ns kotoba.kir-string-index-of-test
  "String search surface (kbb scripts-port wave 2): KIR eval of
  `string-index-of` — UTF-8 byte offset, -1 absent, empty needle traps."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.kir :as ir]))

(defn- run [haystack needle]
  (ir/execute
   {:format :kotoba.kir/v4
    :entry 'main
    :exports ['main]
    :effects #{}
    :functions
    [{:name 'main
      :params []
      :param-types []
      :result :i64
      :effects #{}
      :body (list 'string-index-of haystack needle)}]}
   'main
   []))

(deftest index-of-basics
  (is (= 3 (run "abcdef" "de")))
  (is (= -1 (run "abcdef" "xyz")))
  (is (= 0 (run "abc" "abc")))
  (is (= 1 (run "abcbc" "bc"))))

(deftest index-of-utf8-byte-offsets
  ;; "あ" is 3 UTF-8 bytes; "i" therefore sits at byte offset 3 in "あi".
  (is (= 3 (run "あi" "i")))
  ;; The needle itself is multi-byte and must match by bytes.
  (is (= 0 (run "日本語" "日本")))
  (is (= 3 (run "日本語" "本"))))

(deftest empty-needle-traps
  (try
    (run "abc" "")
    (is false "expected trap")
    (catch clojure.lang.ExceptionInfo e
      (is (= :empty-string-search-needle (:trap (ex-data e)))))))
