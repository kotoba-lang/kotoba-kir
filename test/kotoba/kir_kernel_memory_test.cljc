(ns kotoba.kir-kernel-memory-test
  "The oracle for byte-walking kernel objects.

  Every assertion here is about agreement with `kotoba.native.x86_64` /
  `kotoba.native.aarch64`, not about what a memory model ought to do. Where
  the backends admit something surprising this suite pins the surprise --
  see `four-byte-index-wraps-below-the-buffer`."
  (:require [clojure.test :refer [deftest is]]
            [kotoba.kir :as kir]
            [kotoba.test-hir :as test-hir]))

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
(def ^:private unlock    (module '[base length index] '(kernel-unlock-u32 base length index)))

;; --- the refusal that has to survive ---------------------------------------

(deftest without-an-image-the-refusal-is-unchanged
  (let [data (trapped #(run load-u8 [4096 3 0] {}))]
    (is (= :kernel-memory-unavailable (:trap data)))
    (is (= 'kernel-load-u8 (:operation data)))))

(deftest a-lock-answers-once-someone-supplies-the-word
  ;; This test asserted the opposite, and the reason it changed is the point.
  ;; The refusal was argued from a race -- an image says what the bytes are, a
  ;; lock asks who got there first -- but the operation is a compare-and-swap
  ;; against a comparand and a replacement it fixes itself, over bytes the
  ;; caller wrote, in an interpreter with one thread. That is determined.
  ;;
  ;; What it still cannot model is contention, which is why the assertions
  ;; below are about a free word and a held one and never about two callers.
  ;; The half of the old refusal that was always right is kept by
  ;; `without-an-image-the-lock-still-refuses`.
  (let [mem (image 4096 [0 0 0 0])]
    (is (= 1 (w (run try-lock [4096 4 0] {:memory mem}))))
    (is (= 0 (w (run try-lock [4096 4 0] {:memory mem}))))))

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

;; --- the lock pair, on a supplied image ------------------------------------
;; These model the UNCONTENDED case and nothing else. There is one thread here,
;; so a green vector says the object takes a free lock and releases a held one;
;; it says nothing about two callers.


(deftest a-free-lock-is-taken-and-written
  (let [mem (image 4096 [0 0 0 0])]
    (is (= 1 (w (run try-lock [4096 4 0] {:memory mem}))))
    (is (= [1 0 0 0] @(:bytes mem)))))

(deftest a-held-lock-is-not-taken-and-not-written
  (let [mem (image 4096 [1 0 0 0])]
    (is (= 0 (w (run try-lock [4096 4 0] {:memory mem}))))
    (is (= [1 0 0 0] @(:bytes mem)))))

(deftest a-held-lock-is-released
  (let [mem (image 4096 [1 0 0 0])]
    (is (= 1 (w (run unlock [4096 4 0] {:memory mem}))))
    (is (= [0 0 0 0] @(:bytes mem)))))

(deftest releasing-a-free-lock-changes-nothing
  (let [mem (image 4096 [0 0 0 0])]
    (is (= 0 (w (run unlock [4096 4 0] {:memory mem}))))
    (is (= [0 0 0 0] @(:bytes mem)))))

(deftest a-word-that-is-neither-zero-nor-one-is-not-swapped
  ;; The comparand is exact. A lock word holding anything else is held by
  ;; nobody this operation can name, and both directions leave it alone.
  (let [mem (image 4096 [7 0 0 0])]
    (is (= 0 (w (run try-lock [4096 4 0] {:memory mem}))))
    (is (= 0 (w (run unlock [4096 4 0] {:memory mem}))))
    (is (= [7 0 0 0] @(:bytes mem)))))

(deftest the-lock-ceiling-is-four-kilobytes-not-five-hundred-and-twelve
  ;; `machine-ir` gives the pair `[:gmir/kernel-try-lock-u32 4096]`, so a
  ;; length the u32 loads would refuse is admitted here.
  (let [mem (image 4096 (vec (repeat 1024 0)))]
    (is (= 1 (w (run try-lock [4096 1024 0] {:memory mem}))))
    (let [data (trapped #(run try-lock [4096 4097 0] {:memory mem}))]
      (is (= :length-above-profile-maximum (:check data)))
      (is (= 4096 (:maximum data))))))

(deftest a-lock-needs-four-bytes-after-the-index
  (let [mem (image 4096 [0 0 0 0 0 0])
        data (trapped #(run try-lock [4096 6 3] {:memory mem}))]
    (is (= :kernel-memory-fault (:trap data)))
    (is (= :four-byte-access-outside-window (:check data)))))

(deftest without-an-image-the-lock-still-refuses
  ;; Unchanged, and this is the half of the old refusal that was always right:
  ;; with nothing supplied there is nothing to compare against.
  (let [data (trapped #(run try-lock [4096 4 0] {}))]
    (is (= :kernel-memory-unavailable (:trap data)))
    (is (= 'kernel-try-lock-u32 (:operation data)))))

;; ---------------------------------------------------------------------------
;; sysops: the general atomic family, and the system operations that refuse.
;; ---------------------------------------------------------------------------

(def ^:private add-u32 (module '[base length index delta]
                               '(kernel-atomic-add-u32 base length index delta)))
(def ^:private add-u64 (module '[base length index delta]
                               '(kernel-atomic-add-u64 base length index delta)))
(def ^:private xchg-u32 (module '[base length index new]
                                '(kernel-xchg-u32 base length index new)))
(def ^:private xchg-u64 (module '[base length index new]
                                '(kernel-xchg-u64 base length index new)))
(def ^:private cmpxchg-u32 (module '[base length index expect desire]
                                   '(kernel-cmpxchg-u32 base length index expect desire)))
(def ^:private cmpxchg-u64 (module '[base length index expect desire]
                                   '(kernel-cmpxchg-u64 base length index expect desire)))

(deftest atomic-add-answers-with-the-old-word-and-advances-memory
  ;; `lock xadd` puts the PREVIOUS memory contents in the source register.
  (let [mem (image 4096 [7 0 0 0])]
    (is (= 7 (w (run add-u32 [4096 4 0 5] {:memory mem}))))
    (is (= [12 0 0 0] @(:bytes mem))))
  (let [mem (image 4096 [0xfe 0xff 0xff 0xff])]
    ;; 0xfffffffe + 3 wraps at 32 bits, exactly as `xadd r/m32` does.
    (is (= 4294967294 (w (run add-u32 [4096 4 0 3] {:memory mem}))))
    (is (= [1 0 0 0] @(:bytes mem))))
  (let [mem (image 4096 [1 0 0 0 0 0 0 0])]
    (is (= 1 (w (run add-u64 [4096 8 0 255] {:memory mem}))))
    (is (= [0 1 0 0 0 0 0 0] @(:bytes mem)))))

(deftest exchange-answers-with-the-old-word-and-installs-the-new-one
  (let [mem (image 4096 [1 2 3 4])]
    (is (= 0x04030201 (w (run xchg-u32 [4096 4 0 0xaabbccdd] {:memory mem}))))
    (is (= [0xdd 0xcc 0xbb 0xaa] @(:bytes mem))))
  ;; The eight-byte operand is written 0x0001020304050607 and not
  ;; 0x0102030405060708, and the difference is not cosmetic: on ClojureScript
  ;; a hexadecimal literal is a plain JS number, and 0x0102030405060708 is
  ;; 72,623,859,790,382,856 -- above 2^53, so it is ALREADY rounded to
  ;; 0x0102030405060700 before it reaches the interpreter. The first version of
  ;; this assertion used it and failed on nbb with a zero in byte 0 while
  ;; passing on the JVM, which looked exactly like an encoder bug in the new
  ;; eight-byte helpers and was not.
  ;;
  ;; The top byte is covered by the -2 case below rather than by a bigger
  ;; literal, because there is no eight-byte literal with a non-zero top byte
  ;; that a JS number can hold exactly.
  (let [mem (image 4096 [0 0 0 0 0 0 0 0])]
    (is (= 0 (w (run xchg-u64 [4096 8 0 0x0001020304050607] {:memory mem}))))
    (is (= [7 6 5 4 3 2 1 0] @(:bytes mem))))
  (let [mem (image 4096 [0 0 0 0 0 0 0 0])]
    (is (= 0 (w (run xchg-u64 [4096 8 0 -2] {:memory mem}))))
    (is (= [0xfe 0xff 0xff 0xff 0xff 0xff 0xff 0xff] @(:bytes mem))
        "the top byte of a negative word survives the eight-byte write")))

(deftest compare-exchange-takes-the-comparand-from-the-guest
  ;; The whole difference from `kernel-try-lock-u32`, which fixes 0 -> 1.
  (let [mem (image 4096 [9 0 0 0])]
    (is (= 9 (w (run cmpxchg-u32 [4096 4 0 9 42] {:memory mem})))
        "the observed word comes back whether or not the swap happened")
    (is (= [42 0 0 0] @(:bytes mem)) "a matching comparand swaps"))
  (let [mem (image 4096 [9 0 0 0])]
    (is (= 9 (w (run cmpxchg-u32 [4096 4 0 8 42] {:memory mem}))))
    (is (= [9 0 0 0] @(:bytes mem)) "a mismatching comparand leaves memory alone"))
  (let [mem (image 4096 [5 0 0 0 0 0 0 0])]
    (is (= 5 (w (run cmpxchg-u64 [4096 8 0 5 6] {:memory mem}))))
    (is (= [6 0 0 0 0 0 0 0] @(:bytes mem)))))

(deftest compare-exchange-compares-at-the-operation-width
  ;; `lock cmpxchg` on a doubleword compares EAX, not RAX. A comparand whose
  ;; high half differs must still match.
  (let [mem (image 4096 [9 0 0 0])]
    (is (= 9 (w (run cmpxchg-u32 [4096 4 0 (+ 9 (* 65536 65536)) 42]
                     {:memory mem}))))
    (is (= [42 0 0 0] @(:bytes mem)))))

(deftest eight-byte-atomics-need-eight-bytes-left-in-the-window
  (let [mem (image 4096 [0 0 0 0 0 0 0 0])
        data (trapped #(run add-u64 [4096 8 1 1] {:memory mem}))]
    (is (= :kernel-memory-fault (:trap data)))
    (is (= :eight-byte-access-outside-window (:check data)))
    (is (= 8 (:width data)))
    (is (= [0 0 0 0 0 0 0 0] @(:bytes mem)) "a trapping access writes nothing"))
  ;; and the four-byte reason literal is unchanged for four-byte operations
  (let [mem (image 4096 [0 0 0 0])
        data (trapped #(run add-u32 [4096 4 1 1] {:memory mem}))]
    (is (= :four-byte-access-outside-window (:check data)))
    (is (= 4 (:width data)))))

(deftest the-general-atomics-share-the-lock-pairs-page-ceiling
  (doseq [[m args] [[add-u32 [4096 4097 0 1]]
                    [add-u64 [4096 4097 0 1]]
                    [xchg-u32 [4096 4097 0 1]]
                    [xchg-u64 [4096 4097 0 1]]
                    [cmpxchg-u32 [4096 4097 0 1 2]]
                    [cmpxchg-u64 [4096 4097 0 1 2]]]]
    (let [data (trapped #(run m args {:memory (image 4096 (vec (repeat 16 0)))}))]
      (is (= :kernel-memory-fault (:trap data)))
      (is (= :length-above-profile-maximum (:check data)))
      (is (= 4096 (:maximum data))))))

(deftest without-an-image-the-general-atomics-refuse
  (doseq [[m args op] [[add-u32 [4096 4 0 1] 'kernel-atomic-add-u32]
                       [add-u64 [4096 8 0 1] 'kernel-atomic-add-u64]
                       [xchg-u32 [4096 4 0 1] 'kernel-xchg-u32]
                       [xchg-u64 [4096 8 0 1] 'kernel-xchg-u64]
                       [cmpxchg-u32 [4096 4 0 1 2] 'kernel-cmpxchg-u32]
                       [cmpxchg-u64 [4096 8 0 1 2] 'kernel-cmpxchg-u64]]]
    (let [data (trapped #(run m args {}))]
      (is (= :kernel-memory-unavailable (:trap data)))
      (is (= op (:operation data))))))

(deftest the-system-operations-refuse-because-there-is-nothing-to-answer
  ;; A barrier, the timestamp counter and the GS-base swap refuse for the
  ;; `kernel-cpuid-*` reason, not the `kernel-load-u8` reason: their value (or
  ;; their effect) is a property of the machine this interpreter is not running
  ;; on. Supplying an image changes nothing, which is what the second half
  ;; asserts -- an image says what BYTES are, and none of these read bytes.
  (doseq [op '[kernel-fence-load kernel-fence-store kernel-fence-full
               kernel-rdtsc kernel-rdtscp kernel-swapgs]]
    (let [m (module '[] (list op))]
      (doseq [opts [{} {:memory (image 4096 [0 0 0 0])}]]
        (let [data (trapped #(run m [] opts))]
          (is (= :kernel-privileged-unavailable (:trap data)) op)
          (is (= op (:operation data)) op))))))

(deftest the-new-families-suppress-constant-oracling
  ;; Both mark a module kernel-native, for the reason the MSR pair and the
  ;; `cpuid` four do: without it `lower` would try to fold the operation at
  ;; compile time and abort the compile of a valid program.
  (doseq [body ['(kernel-rdtsc)
                '(kernel-fence-full)
                '(kernel-swapgs)
                '(kernel-atomic-add-u32 4096 4 0 1)
                '(kernel-xchg-u64 4096 8 0 1)
                '(kernel-cmpxchg-u32 4096 4 0 1 2)]]
    (let [lowered (kir/lower
                   (test-hir/module
                    {:format :kotoba.hir/v2 :entry 'main :exports ['main]
                     :result :i64
                     :functions [{:name 'main :params [] :result :i64
                                  :body body}]}))]
      (is (nil? (:oracle-value lowered)) body)
      (is (= [] (:blocks lowered)) body))))
