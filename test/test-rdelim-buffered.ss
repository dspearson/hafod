;;; test-rdelim-buffered.ss -- Large-input equivalence, shared-port interleave
;;; (no data loss), and the set-delimiter timing witness for the delimited readers.
;;;
;;; Three proofs, all PTY-free and self-contained:
;;;   (a) Over a large multi-line input, every reader -- read-line, read-delimited
;;;       (all four delim-actions), read-paragraph, skip-char-set, read-delimited!
;;;       (start/end contract and the buffer-overflow #f return), plus record-reader
;;;       and field-reader -- produces a byte-identical result sequence to an
;;;       INDEPENDENT naive reference computed directly over the string (no rdelim).
;;;   (b) On a SHARED port, interleaving read-line -> skip-char-set -> read-delimited
;;;       -> read-line (and a peek/get-char boundary) loses no character: the reader
;;;       stays strictly char-at-a-time (one get-char / one lookahead-char per step,
;;;       at most one unget-char), so it never over-reads for a following reader.
;;;   (c) Over a large multi-member delimiter set, the real read-delimited loop --
;;;       whose per-char membership is an O(1) bitset load -- is measurably faster
;;;       than an embedded naive reference reader that classifies each char with a
;;;       linear memv scan over the delimiter list. Byte-identical output and a
;;;       positive control (the naive reader is genuinely slower) rule out a vacuous
;;;       witness.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod rdelim) (hafod internal char-sets)
        (hafod field-reader) (chezscheme))

(test-begin "rdelim buffered")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

;; Wall-clock elapsed time of a thunk, in milliseconds. (real-time) is Chez's
;; monotonic-ish millisecond counter -- the in-tree timing idiom (see test/poll.ss
;; and test/test-poll-vacuity.ss), used here for CI-stable coarse deltas.
(define (elapsed-ms thunk)
  (let ((t0 (real-time)))
    (thunk)
    (- (real-time) t0)))

;; Independent reference for read-delimited with the default 'trim action, computed
;; directly over the string (NOT via rdelim). A record is a maximal run of chars for
;; which (delim? c) is false; each delimiter terminates a record; a trailing
;; delimiter is consumed and yields NO trailing empty record (read-delimited returns
;; eof on the following read). This mirrors read-delimited / read-line exactly.
(define (ref-records s delim?)
  (let ((len (string-length s)))
    (let lp ((i 0) (start 0) (acc '()))
      (cond
       ((fx>=? i len)
        (if (fx>? i start)
            (reverse (cons (substring s start i) acc))
            (reverse acc)))
       ((delim? (string-ref s i))
        (lp (fx+ i 1) (fx+ i 1) (cons (substring s start i) acc)))
       (else
        (lp (fx+ i 1) start acc))))))

(define (newline? c) (char=? c #\newline))
(define (colon? c) (char=? c #\:))

;; Independent reference for the default field-splitter: maximal runs of
;; non-whitespace characters, computed directly over the string.
(define (ref-fields s)
  (let ((len (string-length s)))
    (let lp ((i 0) (acc '()))
      (cond
       ((fx>=? i len) (reverse acc))
       ((char-whitespace? (string-ref s i)) (lp (fx+ i 1) acc))
       (else
        (let scan ((j (fx+ i 1)))
          (if (or (fx>=? j len) (char-whitespace? (string-ref s j)))
              (lp j (cons (substring s i j) acc))
              (scan (fx+ j 1)))))))))

;; Drive a port reader to exhaustion, collecting each returned record until eof.
(define (collect-reader read-one port)
  (let lp ((acc '()))
    (let ((r (read-one port)))
      (if (eof-object? r) (reverse acc) (lp (cons r acc))))))

;; ---------------------------------------------------------------------------
;; Large inputs
;; ---------------------------------------------------------------------------

;; Newline-separated lines, each "alpha bravo <i>" (single ASCII spaces, so the
;; default whitespace field-splitter and ref-fields agree), with a trailing newline.
(define (build-lines n)
  (let ((p (open-output-string)))
    (do ((i 0 (fx+ i 1))) ((fx=? i n) (get-output-string p))
      (put-string p "alpha bravo ")
      (put-string p (number->string i))
      (put-char p #\newline))))

;; Colon-separated words with a trailing colon -- a single-char delimiter, so the
;; terminating delimiter is known for the delim-action checks.
(define (build-colon n)
  (let ((p (open-output-string)))
    (do ((i 0 (fx+ i 1))) ((fx=? i n) (get-output-string p))
      (put-string p "word")
      (put-string p (number->string i))
      (put-char p #\:))))

(define lines-input (build-lines 15000))
(define lines-expected (ref-records lines-input newline?))

(define colon-input (build-colon 12000))
(define colon-expected (ref-records colon-input colon?))
(define colon-first (car colon-expected))

;; ---------------------------------------------------------------------------
;; (a) Large-input equivalence -- every reader byte-identical to the reference
;; ---------------------------------------------------------------------------

;; read-line
(test-equal "read-line over a large multi-line input matches the reference"
  lines-expected
  (collect-reader read-line (open-string-input-port lines-input)))

;; read-delimited 'trim (the default action) over the colon input
(test-equal "read-delimited 'trim over a large input matches the reference"
  colon-expected
  (collect-reader (lambda (p) (read-delimited ":" p)) (open-string-input-port colon-input)))

;; read-delimited 'concat -- record includes its terminating delimiter
(test-equal "read-delimited 'concat reproduces the delimiter on the first record"
  (string-append colon-first ":")
  (read-delimited ":" (open-string-input-port colon-input) 'concat))

;; read-delimited 'split -- returns (values record delimiter)
(test-assert "read-delimited 'split returns the record and its delimiter"
  (let-values (((rec delim)
                (read-delimited ":" (open-string-input-port colon-input) 'split)))
    (and (string=? colon-first rec) (char=? #\: delim))))

;; read-delimited 'peek -- leaves the delimiter for a following read
(test-assert "read-delimited 'peek leaves the delimiter in the port"
  (let ((p (open-string-input-port colon-input)))
    (let ((rec (read-delimited ":" p 'peek)))
      (and (string=? colon-first rec) (char=? #\: (lookahead-char p))))))

;; read-delimited with a multi-member char-set delimiter over a large input
(define tokenise-string
  ;; "any non-alphanumeric" delimiter set: space/tab/newline plus every
  ;; non-alphanumeric printable in [33,255]. All members < 256, so the coerced
  ;; char-set is bitset-backed (O(1) membership); the naive reference below scans
  ;; the same members linearly (O(k)).
  (let lp ((cp 255) (acc '()))
    (if (fx<? cp 0)
        (list->string acc)
        (let ((c (integer->char cp)))
          (if (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline)
                  (and (fx>=? cp 33)
                       (not (and (char<=? #\0 c) (char<=? c #\9)))
                       (not (and (char<=? #\a c) (char<=? c #\z)))
                       (not (and (char<=? #\A c) (char<=? c #\Z)))))
              (lp (fx- cp 1) (cons c acc))
              (lp (fx- cp 1) acc))))))
(define tokenise-list (string->list tokenise-string))
(define tokenise-set (string->char-set tokenise-string))
(define (tokenise-delim? c) (and (memv c tokenise-list) #t))

;; A long alphanumeric run (never a delimiter) of the requested length.
(define (a-word len seed)
  (let ((s (make-string len)))
    (do ((i 0 (fx+ i 1))) ((fx=? i len) s)
      (string-set! s i (integer->char (fx+ 97 (mod (fx+ i seed) 26)))))))

;; Long records make per-char membership (bitset O(1) vs memv O(k)) the dominant
;; cost, so the timing witness reflects the membership win rather than record
;; boundary overhead. ~1.6 MB across 8000 records.
(define (build-tokenise word-count word-len)
  (let ((p (open-output-string))
        (delims (list->vector tokenise-list)))
    (do ((i 0 (fx+ i 1))) ((fx=? i word-count) (get-output-string p))
      (put-string p (a-word word-len i))
      (put-char p (vector-ref delims (mod (fx* i 7) (vector-length delims)))))))

(define tokenise-input (build-tokenise 8000 200))
(define tokenise-expected (ref-records tokenise-input tokenise-delim?))

(test-equal "read-delimited with a multi-member char-set matches the reference"
  tokenise-expected
  (collect-reader (lambda (p) (read-delimited tokenise-set p))
                  (open-string-input-port tokenise-input)))

;; read-paragraph -- collect all paragraphs over a multi-paragraph input
(test-equal "read-paragraph collects the paragraphs of a multi-paragraph input"
  '("a\nb\n" "c\nd\n" "e\n")
  (collect-reader read-paragraph
                  (open-string-input-port "a\nb\n\nc\nd\n\n\ne\n")))

;; skip-char-set -- count of skipped chars, and the port lands on the first
;; non-skipped char (proves the lookahead/get-char handoff drops nothing).
(test-assert "skip-char-set skips exactly the leading run and no further"
  (let* ((pad 5000)
         (input (string-append (make-string pad #\space) "tail\nrest\n"))
         (p (open-string-input-port input)))
    (let ((skipped (skip-char-set " " p)))
      (and (= skipped pad)
           (string=? "tail" (read-line p))
           (string=? "rest" (read-line p))))))

;; read-delimited! -- the caller-buffer start/end contract on a large buffer
(test-assert "read-delimited! honours the start/end subrange contract"
  (let ((buf (make-string 20 #\.))
        (p (open-string-input-port "abcdef\n")))
    (let ((n (read-delimited! "\n" buf p 'trim 4 12)))
      (and (= n 6)
           (char=? #\a (string-ref buf 4))
           (char=? #\f (string-ref buf 9))
           ;; Bytes outside [4,10) are untouched.
           (char=? #\. (string-ref buf 3))
           (char=? #\. (string-ref buf 10))))))

;; read-delimited! -- buffer overflow returns #f on a long unterminated record
(test-assert "read-delimited! returns #f on a buffer-overflowing record"
  (let ((buf (make-string 8 #\.))
        (p (open-string-input-port (make-string 5000 #\a))))
    (not (read-delimited! "\n" buf p))))

;; record-reader -- funnels through read-delimited; must match the line reference.
;; This DIRECTLY witnesses the reader family named in the plan rather than covering
;; it only transitively through rdelim.
(test-equal "record-reader over a large input matches the line reference"
  lines-expected
  (let ((rr (record-reader)))
    (collect-reader (lambda (p) (rr p)) (open-string-input-port lines-input))))

;; field-reader -- raw records match the line reference AND each parsed field list
;; matches the independent whitespace field reference. The second explicit witness
;; for the named field-reader / record-reader path.
(let ((fr (field-reader))
      (p (open-string-input-port lines-input)))
  (let lp ((raws '()) (flds '()))
    (let-values (((raw fields) (fr p)))
      (if (eof-object? raw)
          (begin
            (test-equal "field-reader raw records match the line reference"
              lines-expected (reverse raws))
            (test-equal "field-reader parsed fields match the whitespace reference"
              (map ref-fields lines-expected) (reverse flds)))
          (lp (cons raw raws) (cons fields flds))))))

;; ---------------------------------------------------------------------------
;; (b) Shared-port interleave -- no data loss across readers on ONE port
;; ---------------------------------------------------------------------------

;; read-line -> skip-char-set -> read-delimited -> read-line on a single port. Every
;; read is sequenced with let*/begin (Chez evaluates arguments right-to-left, so two
;; reads in one form would read in reverse order). The exact returned sequence proves
;; each reader consumes precisely its own characters and hands the rest on intact.
(test-assert "interleaved readers on one port lose no data"
  (let ((p (open-string-input-port "alpha\n   beta:gamma\nomega\n")))
    (let* ((r1 (read-line p))            ; "alpha", consumes "alpha\n"
           (r2 (skip-char-set " " p))    ; 3, consumes "   "
           (r3 (read-delimited ":" p))   ; "beta", consumes "beta:"
           (r4 (read-line p))            ; "gamma", consumes "gamma\n"
           (r5 (read-line p)))           ; "omega", consumes "omega\n"
      (and (string=? "alpha" r1)
           (= 3 r2)
           (string=? "beta" r3)
           (string=? "gamma" r4)
           (string=? "omega" r5)
           (eof-object? (read-line p))))))

;; The strongest over-read witness: read-delimited 'peek must leave the delimiter,
;; and a following bare get-char must see that exact delimiter char -- if the reader
;; had slurped past ':' the get-char would see 'd' and the sequence would diverge.
(test-assert "read-delimited 'peek then get-char proves the reader never over-reads"
  (let ((p (open-string-input-port "abc:def ghi\njkl\n")))
    (let* ((r1 (read-delimited ":" p 'peek)) ; "abc", leaves ':'
           (c  (get-char p))                 ; #\: -- the very next char
           (r2 (read-delimited " " p))       ; "def", consumes "def "
           (r3 (read-line p))                ; "ghi"
           (r4 (read-line p)))               ; "jkl"
      (and (string=? "abc" r1)
           (char=? #\: c)
           (string=? "def" r2)
           (string=? "ghi" r3)
           (string=? "jkl" r4)))))

;; ---------------------------------------------------------------------------
;; (c) Set-delimiter timing witness -- bitset reader vs naive linear membership
;; ---------------------------------------------------------------------------

;; The embedded naive reference reader (the "before"): strictly char-at-a-time, but
;; it classifies each char with a LINEAR memv scan over the delimiter list -- O(k)
;; per char -- instead of the O(1) bitset load the real reader uses. Its record
;; sequence is identical to read-delimited 'trim (asserted below).
(define (naive-read-delimited delim-list port)
  (let lp ((acc '()))
    (let ((c (get-char port)))
      (cond
       ((eof-object? c) (if (null? acc) c (list->string (reverse acc))))
       ((memv c delim-list) (list->string (reverse acc)))
       (else (lp (cons c acc)))))))

;; Byte-identity of the two port readers over the multi-member set (also confirms
;; the real reader agrees with the string reference above via transitivity).
(define real-records
  (collect-reader (lambda (p) (read-delimited tokenise-set p))
                  (open-string-input-port tokenise-input)))
(define naive-records
  (collect-reader (lambda (p) (naive-read-delimited tokenise-list p))
                  (open-string-input-port tokenise-input)))

(test-assert "the bitset reader and the naive memv reader are byte-identical"
  (equal? real-records naive-records))

;; Warm up both paths, then time each over a fresh port.
(collect-reader (lambda (p) (read-delimited tokenise-set p))
                (open-string-input-port tokenise-input))
(collect-reader (lambda (p) (naive-read-delimited tokenise-list p))
                (open-string-input-port tokenise-input))

(define real-ms
  (elapsed-ms (lambda ()
                (collect-reader (lambda (p) (read-delimited tokenise-set p))
                                (open-string-input-port tokenise-input)))))
(define naive-ms
  (elapsed-ms (lambda ()
                (collect-reader (lambda (p) (naive-read-delimited tokenise-list p))
                                (open-string-input-port tokenise-input)))))

;; Record the measured pair for the summary. The ratio is machine-independent (both
;; readers scale together on slower hardware), so the assertion below is CI-stable.
(printf "~%set-delimiter timing witness: bitset-reader=~ams  naive-memv-reader=~ams  (~a delimiter members, ~a chars, ~a records)~%"
        real-ms naive-ms (length tokenise-list)
        (string-length tokenise-input) (length real-records))

;; The bitset reader beats the naive linear-membership reference by a wide margin.
(test-assert "the bitset reader is more than 4x faster than the naive memv reader"
  (fx<? (fx* 4 real-ms) naive-ms))

;; Positive control: the naive reference is genuinely slower. Without this the
;; timing assertion could pass vacuously (e.g. if both were near zero).
(test-assert "positive control: the naive memv reader is measurably slower"
  (fx>? naive-ms real-ms))

(test-end)
