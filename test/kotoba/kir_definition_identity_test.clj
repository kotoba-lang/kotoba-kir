(ns kotoba.kir-definition-identity-test
  (:require [clojure.test :refer [deftest is testing]]
            [cbor.core :as cbor]
            [kotoba.kir.definition-identity :as identity]))

(def ^:private definition
  {:definition/profile-version 4
   :definition/desugar-contract-version 1
   :definition/kir {:op :const :value 1}
   :definition/effect-row #{}
   :definition/interface {:arity 0}
   :definition/dependencies []})

(def ^:private legacy-canonical-hex
  "82636d6170878282626b776c646570656e64656e636965738263766563808282626b777818646573756761722d636f6e74726163742d76657273696f6e8263696e7461318282626b776a6566666563742d726f778263736574808282626b7769696e7465726661636582636d6170818282626b776561726974798263696e7461308282626b7778226b6f746f62612e646566696e6974696f6e2d6964656e746974792f76657273696f6e8263696e7461328282626b776f70726f66696c652d76657273696f6e8263696e7461348282626b776974797065642d6b697282636d6170828282626b77626f7082626b7765636f6e73748282626b776576616c75658263696e746131")

(deftest extraction-preserves-language-authority-golden
  (is (= legacy-canonical-hex (identity/canonical-hex definition)))
  (is (= "bafyreiewn4op2kytex2iu3cmxeuqjjcttukmm2evq2c7xtynyima5zfk64"
         (identity/definition-cid definition)))
  (is (= (identity/normalize (identity/identity-payload definition))
         (cbor/decode (identity/canonical-bytes definition)))))

(deftest every-semantic-input-remains-sealed
  (let [base (identity/definition-cid definition)]
    (doseq [changed [(assoc definition :definition/profile-version 5)
                     (assoc definition :definition/desugar-contract-version 2)
                     (assoc definition :definition/kir {:op :const :value 2})
                     (assoc definition :definition/effect-row #{:host/http})
                     (assoc definition :definition/interface {:arity 1})]]
      (is (not= base (identity/definition-cid changed))))))

(deftest extracted-literal-wrappers-preserve-the-v2-wire-contract
  (is (= {:kotoba.lang.code-identity/i64 "9007199254740993"}
         (identity/i64 9007199254740993)))
  (is (= ["int" "9007199254740993"]
         (identity/normalize (identity/i64 9007199254740993))))
  (is (= {:kotoba.lang.code-identity/f64 "3ff8000000000000"}
         (identity/f64 1.5)))
  (is (= ["f64" "3ff8000000000000"]
         (identity/normalize (identity/f64 1.5)))))

(deftest canonical-domain-fails-closed
  (testing "map and set source order is not identity"
    (is (= (identity/normalize {:a 1 :b 2})
           (identity/normalize (array-map :b 2 :a 1))))
    (is (= (identity/normalize #{:a :b})
           (identity/normalize #{:b :a}))))
  (testing "unknown and inexact host values are rejected"
    (is (thrown? clojure.lang.ExceptionInfo (identity/normalize (java.util.Date.))))
    (is (thrown? clojure.lang.ExceptionInfo (identity/normalize 1.5)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (identity/normalize 9007199254740993)))))
