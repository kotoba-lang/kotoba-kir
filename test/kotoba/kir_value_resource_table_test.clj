(ns kotoba.kir-value-resource-table-test
  "Linear resource table for task/stream handles — ADR 0133."
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.kir.value :as value]))

(deftest construction-registers-live-handles
  (value/resource-table-reset!)
  (let [stream (value/make-bytes-stream (byte-array [1 2]))
        task (value/make-ready-bytes-task (byte-array [3]))]
    (is (true? (value/stream-live? stream)))
    (is (true? (value/task-live? task)))
    (is (true? (value/stream-live? (:kotoba.task/stream task))))))

(deftest drop-then-ops-fail-closed
  (value/resource-table-reset!)
  (let [task (value/make-ready-bytes-task (value/utf8-string->bytes "ab"))
        stream (:kotoba.task/stream task)]
    (value/task-drop! task)
    (is (false? (value/task-live? task)))
    (is (false? (value/stream-live? stream)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not live"
                          (value/task-poll task)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not live"
                          (value/stream-read! stream 10)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not live"
                          (value/task-drop! task)))))

(deftest stream-drop-independent-of-task
  (value/resource-table-reset!)
  (let [stream (value/make-bytes-stream (byte-array [9]))]
    (value/stream-drop! stream)
    (is (false? (value/stream-live? stream)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not live"
                          (value/stream-read! stream 1)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not live"
                          (value/stream-drop! stream)))))

(deftest fulfill-registers-new-stream
  (value/resource-table-reset!)
  (let [pending (value/make-pending-bytes-task)
        ready (value/task-fulfill! pending (byte-array [1 2 3]))
        stream (:kotoba.task/stream ready)]
    (is (true? (value/task-live? pending)))
    (is (true? (value/task-live? ready)))
    (is (= (:kotoba.task/id pending) (:kotoba.task/id ready)))
    (is (true? (value/stream-live? stream)))
    (let [chunk (value/stream-read! stream 10)]
      (is (true? (:done? chunk)))
      (is (= 3 (value/bytes-byte-count (:bytes chunk)))))))

(deftest progressive-ops-require-live
  (value/resource-table-reset!)
  (let [task (value/make-ready-open-chunk-queue-task)
        stream (:kotoba.task/stream task)]
    (value/stream-enqueue! stream (byte-array [1]))
    (value/stream-close! stream)
    (is (true? (:done? (value/stream-read! stream 10))))
    (value/task-drop! task)
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not live"
                          (value/stream-enqueue! stream (byte-array [2]))))))
