(ns kotoba.kir-kernel-memory-test
  "The oracle for byte-walking kernel objects.

  Every assertion here is about agreement with `kotoba.native.x86_64` /
  `kotoba.native.aarch64`, not about what a memory model ought to do. Where
  the backends admit something surprising this suite pins the surprise --
  see `four-byte-index-wraps-below-the-buffer`."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.kir :as kir]))

(defn- module [params body]
  {:format :kotoba.kir/v4
   :entry 'main
   :effects #{}
   :functions [{:name 'main :params params
                :param-types (vec (repeat (count params) :i64))
                :result :i64 :effects #{} :body body}]})

(defn- image [base bytes] {:base base :bytes (volatile! bytes)})

(defn- run [m args opts] (kir/execute m 'main args opts))

(defn- trapped [thunk]
  (try (thunk) nil
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e (ex-data e))))

(defn- w [x] #?(:clj x :cljs (js/Number x)))

(def ^:private load-u8   (module '[base length index] '(kernel-load-u8 base length index)))
(def ^:private load-4k   (module '[base length index] '(kernel-load-u8-4k base length index)))
(def ^:private store-u8  (module '[base length index value]
                                 '(kernel-store-u8 base length index value)))
(def ^:private load-u32  (module '[base length index] '(kernel-load-u32 base length index)))
(def ^:private store-u32 (module '[base length index value]
                                 '(kernel-store-u32 base length index value)))
(def ^:private subregion (module '[base length offset sublen]
                                 '(kernel-subregion base length offset sublen)))
(def ^:private try-lock  (module '[base length index] '(kernel-try-lock-u32 base length index)))

;; --- the refusal that has to survive ---------------------------------------

(deftest without-an-image-the-refusal-is-unchanged
  (let [data (trapped #(run load-u8 [4096 3 0] {}))]
    (is (= :kernel-memory-unavailable (:trap data)))
    (is (= 'kernel-load-u8 (:operation data)))))

(deftest a-lock-refuses-even-with-an-image
  ;; An image says what the bytes ARE. A lock asks who got there first.
  (let [data (trapped #(run try-lock [4096 4 0] {:memory (image 4096 [0 0 0 0])}))]
    (is (= :kernel-memory-unavailable (:trap data)))))

;; --- reads and writes ------------------------------------------------------

(deftest a-load-returns-the-supplied-byte
  (is (= 20 (w (run load-u8 [4096 3 1] {:memory (image 4096 [10 20 30])})))))

(deftest a-store-is-visible-to-the-caller
  (let [mem (image 4096 [10 20 30])
        result (run store-u8 [4096 3 2 99] {:memory mem})]
    (is (= [10 20 99] @(:bytes mem)))
    ;; RAX still holds `value` after `mov [rdx+rdi],al`.
    (is (= 99 (w result)))))

(deftest a-store-narrows-to-one-byte
  (let [mem (image 4096 [0])]
    (run store-u8 [4096 1 0 511] {:memory mem})
    (is (= [255] @(:bytes mem)))))

(deftest u32-is-little-endian
  (is (= 0x04030201 (w (run load-u32 [4096 8 0]
                              {:memory (image 4096 [1 2 3 4 0 0 0 0])})))))

(deftest a-u32-store-writes-four-little-endian-bytes
  (let [mem (image 4096 [0 0 0 0 0 0 0 0])]
    (run store-u32 [4096 8 2 0x04030201] {:memory mem})
    (is (= [0 0 1 2 3 4 0 0] @(:bytes mem)))))

;; --- the three checks each backend emits, in their order --------------------

(deftest length-above-the-profile-maximum-faults
  (let [mem (image 4096 (vec (repeat 600 0)))
        data (trapped #(run load-u8 [4096 513 0] {:memory mem}))]
    (is (= :kernel-memory-fault (:trap data)))
    (is (= :length-above-profile-maximum (:check data)))
    (is (= 512 (:maximum data))))
  ;; The same length on the 4 KiB profile is admitted -- the bound is the
  ;; operation's, not the image's.
  (is (= 7 (w (run load-4k [4096 513 0] {:memory (image 4096 (into [7] (repeat 599 0)))})))))

(deftest a-null-base-faults
  (let [data (trapped #(run load-u8 [0 3 0] {:memory (image 4096 [1 2 3])}))]
    (is (= :kernel-memory-fault (:trap data)))
    (is (= :null-base (:check data)))))

(deftest an-index-at-the-window-edge-faults
  (let [mem (image 4096 [1 2 3 4])]
    (is (= 3 (w (run load-u8 [4096 3 2] {:memory mem}))))
    (let [data (trapped #(run load-u8 [4096 3 3] {:memory mem}))]
      (is (= :kernel-memory-fault (:trap data)))
      (is (= :index-outside-window (:check data))))))

(deftest a-negative-index-is-a-huge-unsigned-one
  (let [data (trapped #(run load-u8 [4096 3 -1] {:memory (image 4096 [1 2 3])}))]
    (is (= :kernel-memory-fault (:trap data)))
    (is (= :index-outside-window (:check data)))))

(deftest a-four-byte-access-needs-four-bytes
  (let [mem (image 4096 [1 2 3 4 5 6])]
    (is (= 0x05040302 (w (run load-u32 [4096 6 1] {:memory mem}))))
    (let [data (trapped #(run load-u32 [4096 6 3] {:memory mem}))]
      (is (= :kernel-memory-fault (:trap data)))
      (is (= :four-byte-access-outside-window (:check data))))))

(deftest a-four-byte-index-near-two-to-the-sixty-four-faults
  ;; This test asserted the opposite for one commit, and the story is worth
  ;; keeping. `kotoba.native.x86-64/emit-kernel-load-u32` computes `index + 4`
  ;; with `lea` and compares THAT against length, so an index in
  ;; [2^64-4, 2^64-1] wraps to 0..3 and addresses the four bytes BEFORE the
  ;; window. Reproducing it here was justified on the grounds that an oracle
  ;; must never refuse what the machine admits.
  ;;
  ;; The grounds were right; the reading was not. `emit-program` routes u32
  ;; through `kotoba.native.machine-ir`, which proves `index < length` and then
  ;; `length - index >= 4` -- a form that cannot wrap, pinned since before this
  ;; file existed by `u32-accesses-reserve-four-bytes-not-one`. Those two
  ;; emitters are a fallback nothing reached (measured 2026-08-31 by
  ;; instrumenting both vars, recursive walker included), and they now carry
  ;; the same guard.
  ;;
  ;; So the oracle was wrong in the OTHER direction for one commit: it admitted
  ;; four indexes the machine refuses, which is how a Kotoba object passes an
  ;; oracle and traps on hardware.
  (let [mem (image 4096 [9 9 9 1 2 3 4 9 9 9 9 9 9 9 9 9])
        data (trapped #(run load-u32 [4100 8 -1] {:memory mem}))]
    (is (= :kernel-memory-fault (:trap data)))
    (is (= :index-outside-window (:check data)))))

;; --- subregion --------------------------------------------------------------

(deftest a-subregion-is-a-checked-pointer
  (let [mem (image 4096 (vec (repeat 64 0)))]
    (is (= 4104 (w (run subregion [4096 32 8 16] {:memory mem}))))
    (is (= :offset-outside-window
           (:check (trapped #(run subregion [4096 32 33 1] {:memory mem})))))
    (is (= :subwindow-outside-window
           (:check (trapped #(run subregion [4096 32 8 25] {:memory mem})))))
    (is (= :null-base
           (:check (trapped #(run subregion [0 32 0 1] {:memory mem})))))))

;; --- could not answer is neither verdict ------------------------------------

(deftest an-access-the-image-does-not-cover-is-not-a-refusal
  ;; Legal for the machine: length 3 <= 512, base non-null, index 2 < 3. The
  ;; image simply does not reach that far. That is the oracle failing to
  ;; answer, and it must not be readable as either a fault or an admission.
  (let [data (trapped #(run load-u8 [4096 3 2] {:memory (image 4096 [1 2])}))]
    (is (= :kernel-memory-outside-image (:trap data)))
    (is (not= :kernel-memory-fault (:trap data)))))

;; --- the image contract -----------------------------------------------------

(deftest a-plain-vector-is-refused-because-its-stores-would-be-invisible
  (is (some? (trapped #(kir/execute load-u8 'main [4096 3 0]
                                    {:memory {:base 4096 :bytes [1 2 3]}})))))

(deftest a-zero-base-image-is-refused
  (is (some? (trapped #(kir/execute load-u8 'main [4096 3 0]
                                    {:memory (image 0 [1 2 3])})))))

(deftest a-byte-outside-0-255-is-refused
  (is (some? (trapped #(kir/execute load-u8 'main [4096 3 0]
                                    {:memory (image 4096 [1 2 256])})))))
