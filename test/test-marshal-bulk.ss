;;; test-marshal-bulk.ss -- Byte-identical equivalence oracle + throughput witness
;;; for the bulk-memcpy marshalling of ptr->string / posix-read / posix-write.
;;;
;;; These three functions marshal bytes between a foreign buffer and a Scheme
;;; bytevector. This suite embeds the pre-fix per-byte foreign-ref/foreign-set!
;;; loops verbatim as the "before" reference, then proves two things at once:
;;;
;;;   1. Correctness -- the real functions produce byte-identical results to the
;;;      naive reference across empty, one-byte, multibyte-UTF-8 and short-read
;;;      cases. This equivalence oracle is also the FFI memory-safety regression
;;;      test: a memcpy that over-read the foreign buffer or over-wrote the
;;;      bytevector would diverge here (extra/By missing/wrong bytes).
;;;
;;;   2. Throughput -- on a >= 1 MiB buffer the real ptr->string clears the naive
;;;      foreign-ref loop by a wide margin. A positive control (identical decoded
;;;      strings of the full length) rules out a vacuous win: the fast path does
;;;      the whole job, only faster.
;;;
;;; The timing idiom is the in-tree (real-time) ms delta -- as test-poll-vacuity.ss
;;; and test/poll.ss use -- not current-time. PTY-free; a real pipe carries the
;;; posix-read/posix-write round-trips. British-English throughout.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod internal posix-core) (chezscheme))

(test-begin "marshal-bulk")

;; ---------------------------------------------------------------------------
;; The embedded "before" reference: the pre-fix per-byte marshalling loops.
;; ---------------------------------------------------------------------------

;; Verbatim pre-fix ptr->string: a per-byte foreign-ref NUL length-scan, a
;; per-byte foreign-ref copy, then the identical utf8->string decode.
(define (naive-ptr->string ptr)
  (let ((len (let loop ((i 0))
               (if (= (foreign-ref 'unsigned-8 ptr i) 0) i (loop (+ i 1))))))
    (let ((bv (make-bytevector len)))
      (do ((i 0 (+ i 1))) ((= i len))
        (bytevector-u8-set! bv i (foreign-ref 'unsigned-8 ptr i)))
      (utf8->string bv))))

;; The pre-fix posix-write inner loop in isolation: a bytevector copied into a
;; fresh foreign buffer via per-byte foreign-set!. Caller frees the returned
;; address. (A zero length is allocated as one byte purely so this reference
;; helper never itself trips foreign-alloc's positive-fixnum guard; no empty
;; bytevector is ever routed through it.)
(define (naive-marshal-out bv)
  (let* ((len (bytevector-length bv))
         (buf (foreign-alloc (if (= len 0) 1 len))))
    (do ((i 0 (+ i 1))) ((= i len))
      (foreign-set! 'unsigned-8 buf i (bytevector-u8-ref bv i)))
    buf))

;; The pre-fix posix-read inner loop in isolation: a foreign buffer copied into a
;; fresh bytevector via per-byte foreign-ref.
(define (naive-marshal-in buf n)
  (let ((bv (make-bytevector n)))
    (do ((i 0 (+ i 1))) ((= i n))
      (bytevector-u8-set! bv i (foreign-ref 'unsigned-8 buf i)))
    bv))

;; A naive foreign round-trip (out then back in) -- the embedded reference that
;; the real posix-write + posix-read round-trip must match byte-for-byte.
(define (naive-roundtrip bv)
  (let* ((len (bytevector-length bv))
         (buf (naive-marshal-out bv))
         (out (naive-marshal-in buf len)))
    (foreign-free buf)
    out))

;; In-tree timing idiom: elapsed real time in ms around a thunk.
(define (elapsed-ms thunk)
  (let ((t0 (real-time)))
    (thunk)
    (- (real-time) t0)))

;; Allocate a NUL-terminated foreign buffer holding payload's bytes, run proc on
;; its address, then free it -- even on a non-local exit.
(define (with-cstring payload proc)
  (let* ((n (bytevector-length payload))
         (addr (foreign-alloc (+ n 1))))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (do ((i 0 (+ i 1))) ((= i n))
          (foreign-set! 'unsigned-8 addr i (bytevector-u8-ref payload i)))
        (foreign-set! 'unsigned-8 addr n 0)
        (proc addr))
      (lambda () (foreign-free addr)))))

;; Round-trip a bytevector through a real pipe with the REAL posix-write /
;; posix-read; returns (cons bytes-read bytes-written). The let* sequences the
;; write strictly before the read -- Chez evaluates arguments right-to-left, so a
;; single form must never hold both side-effecting calls.
(define (pipe-rt bv read-count)
  (let* ((fds (posix-pipe))
         (rfd (car fds))
         (wfd (cdr fds))
         (nw  (posix-write wfd bv))
         (got (posix-read rfd read-count)))
    (posix-close rfd)
    (posix-close wfd)
    (cons got nw)))

;; A multibyte-UTF-8 payload built from explicit code points (ASCII-only source,
;; so the file's own encoding cannot skew the test): "caf", U+00E9 (2 bytes),
;; U+2192 (3 bytes), U+03BB (2 bytes), U+65E5 (3 bytes). No 0x00 byte, so the
;; NUL length-scan runs to the terminator.
(define multibyte-string
  (list->string (list #\c #\a #\f
                      (integer->char #xE9)      ; e-acute   U+00E9  (2 bytes)
                      (integer->char #x2192)    ; rightward U+2192  (3 bytes)
                      (integer->char #x3BB)     ; lambda    U+03BB  (2 bytes)
                      (integer->char #x65E5)))) ; CJK       U+65E5  (3 bytes)

;; ===========================================================================
;; Equivalence oracle -- ptr->string (also the memory-safety regression test)
;; ===========================================================================

;; Empty C string: strlen 0, memcpy(_,_,0) no-op, decodes to "".
(with-cstring (string->utf8 "")
  (lambda (addr)
    (let* ((naive (naive-ptr->string addr))
           (real  (ptr->string addr)))
      (test-equal "ptr->string empty: real equals the naive reference" naive real)
      (test-equal "ptr->string empty: decodes to the empty string" "" real))))

;; Pure ASCII.
(with-cstring (string->utf8 "hello world")
  (lambda (addr)
    (let* ((naive (naive-ptr->string addr))
           (real  (ptr->string addr)))
      (test-equal "ptr->string ASCII: real equals the naive reference" naive real)
      (test-equal "ptr->string ASCII: decodes correctly" "hello world" real))))

;; Multibyte UTF-8 (2- and 3-byte sequences).
(with-cstring (string->utf8 multibyte-string)
  (lambda (addr)
    (let* ((naive (naive-ptr->string addr))
           (real  (ptr->string addr)))
      (test-equal "ptr->string multibyte: real equals the naive reference" naive real)
      (test-equal "ptr->string multibyte: decodes correctly" multibyte-string real))))

;; ===========================================================================
;; Equivalence oracle -- posix-read / posix-write byte-identity
;; ===========================================================================

;; One byte.
(let* ((msg (make-bytevector 1 65))          ; #vu8(65) = "A"
       (ref (naive-roundtrip msg))
       (rt  (pipe-rt msg 1))
       (got (car rt))
       (nw  (cdr rt)))
  (test-equal "posix-write one byte: returns write count 1" 1 nw)
  (test-equal "posix-read one byte: byte-identical to what was written" msg got)
  (test-equal "posix-read one byte: equals the embedded naive round-trip" ref got))

;; ASCII.
(let* ((msg (string->utf8 "hello"))
       (ref (naive-roundtrip msg))
       (rt  (pipe-rt msg (bytevector-length msg)))
       (got (car rt))
       (nw  (cdr rt)))
  (test-equal "posix-write ASCII: returns the full write count" (bytevector-length msg) nw)
  (test-equal "posix-read ASCII: byte-identical to what was written" msg got)
  (test-equal "posix-read ASCII: equals the embedded naive round-trip" ref got))

;; Multibyte UTF-8.
(let* ((msg (string->utf8 multibyte-string))
       (ref (naive-roundtrip msg))
       (rt  (pipe-rt msg (bytevector-length msg)))
       (got (car rt))
       (nw  (cdr rt)))
  (test-equal "posix-write multibyte: returns the full write count" (bytevector-length msg) nw)
  (test-equal "posix-read multibyte: byte-identical to what was written" msg got)
  (test-equal "posix-read multibyte: equals the embedded naive round-trip" ref got))

;; Deterministic short read: write K bytes, then ask for more than K. The read
;; must return exactly the K bytes available -- the actual short-read count, not
;; the requested count -- proving posix-read sizes its result to the syscall's
;; returned n. K is well under PIPE_BUF, so the write is atomic and all K bytes
;; are buffered before the read.
(let* ((msg (string->utf8 "shortread!"))     ; 10 bytes
       (k   (bytevector-length msg))
       (rt  (pipe-rt msg (+ k 90)))          ; request 100
       (got (car rt))
       (nw  (cdr rt)))
  (test-equal "posix-write short-read case: wrote all K bytes" k nw)
  (test-equal "posix-read short read: result length is the actual count, not the request"
    k (bytevector-length got))
  (test-equal "posix-read short read: bytes are exactly those written" msg got))

;; Empty read via EOF: close the write end, then read. The syscall returns 0, so
;; posix-read sizes a zero-length bytevector and memcpy(_,_,0) is the no-op path.
(let* ((fds (posix-pipe))
       (rfd (car fds))
       (wfd (cdr fds)))
  (posix-close wfd)
  (let ((got (posix-read rfd 8)))
    (posix-close rfd)
    (test-equal "posix-read at EOF: empty bytevector (n=0 memcpy no-op path)"
      (make-bytevector 0) got)
    (test-equal "posix-read at EOF: zero length" 0 (bytevector-length got))))

;; Empty write: the with-foreign-buffer scaffold calls foreign-alloc(0), which
;; Chez rejects (0 is not a positive fixnum). This bulk-copy change leaves that
;; scaffold untouched, so the outcome stays byte-identical -- a raise, exactly as
;; before. The pipe's descriptors are closed on the way out.
(test-error "posix-write of an empty bytevector raises (unchanged foreign-alloc(0) outcome)"
  (let* ((fds (posix-pipe))
         (rfd (car fds))
         (wfd (cdr fds)))
    (dynamic-wind
      (lambda () #f)
      (lambda () (posix-write wfd (make-bytevector 0)))
      (lambda () (posix-close rfd) (posix-close wfd)))))

;; ===========================================================================
;; Throughput witness -- ptr->string over a >= 1 MiB buffer
;; ===========================================================================
;; Sited on ptr->string: a pure marshal with no syscall, so it isolates the copy
;; cost. RED pre-impl (the real function IS the naive loop today, ~1x); GREEN
;; post-impl (one vectorised memcpy vs a >= 1 MiB Scheme loop -- measured ~18x
;; on the flake Chez). The 4x threshold is deliberately generous so a true win
;; clears it without CI flakiness.
(let* ((payload (* 1024 1024))               ; exactly 1 MiB
       (addr    (foreign-alloc (+ payload 1))))
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (do ((i 0 (+ i 1))) ((= i payload))
        (foreign-set! 'unsigned-8 addr i 65))   ; 'A' -- non-zero, so no early NUL
      (foreign-set! 'unsigned-8 addr payload 0)  ; terminating NUL => strlen = payload
      (let ((reps 4))
        ;; Warm pages and caches once on each path before timing.
        (ptr->string addr)
        (naive-ptr->string addr)
        (let* ((real-ms  (elapsed-ms
                           (lambda () (do ((k 0 (+ k 1))) ((= k reps)) (ptr->string addr)))))
               (naive-ms (elapsed-ms
                           (lambda () (do ((k 0 (+ k 1))) ((= k reps)) (naive-ptr->string addr)))))
               (real-str  (ptr->string addr))
               (naive-str (naive-ptr->string addr)))
          (display "  [marshal-witness] ptr->string ")
          (display (quotient payload 1024)) (display " KiB x") (display reps)
          (display " reps: naive-ms=") (display naive-ms)
          (display " memcpy-ms=") (display real-ms)
          (display " (per-call naive=") (display (/ naive-ms (exact->inexact reps)))
          (display " memcpy=") (display (/ real-ms (exact->inexact reps))) (display ")")
          (newline)
          ;; Positive control: both paths decode the identical, full-length
          ;; string -- the fast path does the whole job, not less of it.
          (test-assert "ptr->string >=1MiB: memcpy and naive decode byte-identically"
            (and (string=? real-str naive-str)
                 (= (string-length real-str) payload)))
          ;; The measured witness -- RED pre-impl, GREEN post-impl.
          (test-assert "ptr->string >=1MiB: memcpy path beats the naive foreign-ref loop by >4x"
            (< real-ms (/ naive-ms 4.0))))))
    (lambda () (foreign-free addr))))

(test-end)
