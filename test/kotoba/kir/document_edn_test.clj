(ns kotoba.kir.document-edn-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.kir.value :as value]))

(def document
  ["map"
   [[:attempt ["i64" -7]]
    [:goal ["string" "言葉\n移行"]]
    [:ratio ["f64" 1.5]]
    [:ready ["bool" true]]
    [:steps ["vector" [["null"] ["keyword" :actor/run] ["symbol" 'worker/step]]]]]])

(deftest bounded-document-textual-edn-roundtrip
  (let [printed (value/document-edn-print document)]
    (is (= "{:attempt -7 :goal \"言葉\\n移行\" :ratio 1.5 :ready true :steps [nil :actor/run worker/step]}"
           printed))
    (is (= (value/bounded-document! document)
           (value/document-edn-read printed)))
    (is (= ["map" [[:a ["i64" 1]] [:b ["bool" false]]]]
           (value/document-edn-read "; bounded policy\n{:b false, :a 1}")))
    (is (= ["symbol" 'actor/run]
           (value/document-edn-read "actor/run")))
    (is (= ["symbol" 'actor/run]
           (value/document-read (value/document-print ["symbol" 'actor/run]))))))

(deftest textual-edn-printer-rejects-ambiguous-symbols
  (doseq [item [(symbol "nil") (symbol "true") (symbol "42")
                (symbol ":keyword") (symbol "#tag")]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"ambiguous symbol"
                          (value/document-edn-print ["symbol" item])))))

(deftest textual-edn-reader-fails-closed
  (doseq [[label input]
          [["empty" ""]
           ["trailing" "nil true"]
           ["tag" "#inst \"2026-08-03\""]
           ["discard" "#_ nil"]
           ["set" "#{:a}"]
           ["list" "(1 2)"]
           ["non-keyword map key" "{\"a\" 1}"]
           ["duplicate map key" "{:a 1 :a 2}"]
           ["missing map value" "{:a}"]
           ["i64 overflow" "9223372036854775808"]
           ["non-finite" "1e999"]]]
    (testing label
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"document-edn-read"
                            (value/document-edn-read input))))))
