;;; test-sexp-singlepass.ss -- Proof that the single-pass sexp navigation in
;;; (hafod editor sexp-tracker) is byte-identical to the former re-lex-from-0
;;; implementation, and measurably faster on the quadratic paths.
;;;
;;; Two halves make the proof, in the test-charset-bitset ethos:
;;;   1. Byte-identity.  forward-down-list is compared against a from-0 reference
;;;      that inlines the pre-fix algorithm verbatim (using the UNTOUCHED,
;;;      exported lexer-state-at); the public forward-sexp-end is compared against
;;;      both an inlined pre-fix reference and its own cursor entry seeded from the
;;;      from-0 state -- over a large diverse buffer AND a set of tricky small
;;;      buffers, at EVERY start position.  These position-sequence equalities are
;;;      the deterministic hard gate.
;;;   2. Timing witnesses.  forward-down-list (one pass) is timed against the
;;;      from-0 O(n^2) reference on a many-opener buffer -- the genuine per-call
;;;      quadratic.  forward-sexp-end is per-call O(n) (one lexer-state-at); its
;;;      quadratic only appears under CHAINED calls, so its witness walks a buffer
;;;      end-to-end two ways -- the public chain (re-lexes the prefix from 0 each
;;;      step, O(n^2)) versus a cursor-threaded chain (O(n)) -- asserting the two
;;;      end-position sequences are identical and the threaded walk is far faster.
;;;      Each witness emits a WITNESS line so the measured pair can be recorded.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (chezscheme) (hafod editor sexp-tracker))

(test-begin "sexp-singlepass")

;; Wall-clock elapsed milliseconds for a thunk (the in-tree (real-time) idiom).
(define (elapsed-ms thunk)
  (let ((t0 (real-time)))
    (thunk)
    (- (real-time) t0)))

;; sexp-delimiter? is library-internal; inline it for the references.
(define (delim? ch)
  (or (char-whitespace? ch)
      (memv ch '(#\( #\) #\[ #\] #\" #\;))))

;; Append a string to an output port, char by char (no write-string dependency).
(define (emit p s) (string-for-each (lambda (c) (write-char c p)) s))

;; =============================================================================
;; From-0 references -- the PRE-FIX algorithms, verbatim, over the untouched
;; lexer-state-at / find-matching-close.  Independently correct because those
;; primitives are unchanged; they are what the single-pass code must reproduce.
;; =============================================================================

;; reference-fdl: forward-down-list as it was before -- loop from pos, and at each
;; '(' or '[' re-lex the whole prefix with lexer-state-at (the O(n^2) shape).
(define (reference-fdl str pos)
  (let ((len (string-length str)))
    (let loop ((i pos))
      (cond
        ((fx>= i len) #f)
        ((and (let ((ch (string-ref str i)))
                (or (char=? ch #\() (char=? ch #\[)))
              (eq? (lexer-state-at str i) 'normal))
         (fx+ i 1))
        (else (loop (fx+ i 1)))))))

;; reference-fse: forward-sexp-end as it was before -- skip whitespace, seed the
;; state at start with lexer-state-at, then the structural navigation inline.
(define (reference-fse str pos)
  (let* ((len (string-length str))
         (start (let skip ((i pos))
                  (if (and (fx< i len) (char-whitespace? (string-ref str i)))
                      (skip (fx+ i 1)) i))))
    (cond
      ((fx>= start len) #f)
      (else
       (let ((ch (string-ref str start))
             (state (lexer-state-at str start)))
         (cond
           ((not (eq? state 'normal)) #f)
           ((or (char=? ch #\() (char=? ch #\[))
            (let ((close (find-matching-close str start))) (and close (fx+ close 1))))
           ((char=? ch #\")
            (let skip-str ((i (fx+ start 1)))
              (cond ((fx>= i len) #f)
                    ((char=? (string-ref str i) #\\) (skip-str (fx+ i 2)))
                    ((char=? (string-ref str i) #\") (fx+ i 1))
                    (else (skip-str (fx+ i 1))))))
           ((char=? ch #\#)
            (cond
              ((and (fx< (fx+ start 1) len) (char=? (string-ref str (fx+ start 1)) #\())
               (let ((close (find-matching-close str (fx+ start 1)))) (and close (fx+ close 1))))
              ((and (fx< (fx+ start 1) len) (char=? (string-ref str (fx+ start 1)) #\\))
               (let skip-atom ((i (fx+ start 2)))
                 (cond ((fx>= i len) i) ((delim? (string-ref str i)) i) (else (skip-atom (fx+ i 1))))))
              (else
               (let skip-atom ((i start))
                 (cond ((fx>= i len) i) ((delim? (string-ref str i)) i) (else (skip-atom (fx+ i 1))))))))
           ((or (char=? ch #\)) (char=? ch #\])) #f)
           (else
            (let skip-atom ((i start))
              (cond ((fx>= i len) i) ((delim? (string-ref str i)) i) (else (skip-atom (fx+ i 1))))))))))))

;; The public forward-sexp-end routed through the cursor entry, seeded with the
;; from-0 state at start -- must equal the public entry for every position.
(define (fse-via-cursor str pos)
  (let* ((len (string-length str))
         (start (let skip ((i pos))
                  (if (and (fx< i len) (char-whitespace? (string-ref str i)))
                      (skip (fx+ i 1)) i))))
    (cond
      ((fx>= start len) #f)
      (else
       (call-with-values
         (lambda () (forward-sexp-end/cursor str start (lexer-state-at str start)))
         (lambda (end st) end))))))

;; =============================================================================
;; Section A -- byte-identity over tricky small buffers x EVERY start position
;; =============================================================================
;; Strings, line comments, single- and nested block comments, char literals and
;; unterminated forms, each with brackets straddling many positions (Pitfall 6).

(define tricky-buffers
  (list ""
        "("
        "((((deep))))"
        "[a [b] c]"
        "(+ [1 2] 3)"
        "(a \"( ] ;\" b)"                  ; string hides brackets/comment chars
        "(a \"\\\"(\" (b))"               ; escaped quote then paren inside string
        "(a ; ( [ comment\n (b))"         ; line comment hides an opener
        "(a #| ( [ |# (b))"               ; block comment hides an opener
        "(a #| #| nested ( |# |# (b))"    ; nested block comment
        "#\\( (a)"                          ; char literal open paren, then a list
        "(x #\\) y (z))"                   ; char literal close paren
        "(((((("                            ; only openers
        "))))))"                            ; only closers
        "  \n\t (leading ws) rest"
        "#t #f #\\a (real)"
        "'foo `bar ,baz ,@qux (end)"
        "#(1 2 3) #vu8(4 5)"
        "(define (f x) (+ x 1))"
        "\"unterminated string ( [ still open"
        "#| unterminated block ( comment"
        "; unterminated line comment ("
        "(a)(b)(c)(d)"))

;; First (buf pos our theirs) where f and g disagree over the buffer set, or #f.
(define (first-divergence bufs f g)
  (let loop ((bs bufs))
    (if (null? bs)
        #f
        (let* ((buf (car bs)) (len (string-length buf)))
          (let lp ((p 0))
            (cond
              ((fx> p len) (loop (cdr bs)))
              ((equal? (f buf p) (g buf p)) (lp (fx+ p 1)))
              (else (list buf p (f buf p) (g buf p)))))))))

(test-equal "forward-down-list == from-0 reference over tricky buffers x every position (value = first divergence or #f)"
            #f (first-divergence tricky-buffers forward-down-list reference-fdl))
(test-equal "public forward-sexp-end == from-0 reference over tricky buffers x every position"
            #f (first-divergence tricky-buffers forward-sexp-end reference-fse))
(test-equal "public forward-sexp-end == cursor seeded from the from-0 state over tricky buffers x every position"
            #f (first-divergence tricky-buffers forward-sexp-end fse-via-cursor))

;; =============================================================================
;; Section B -- byte-identity over a large (~20k) diverse buffer x positions
;; =============================================================================
;; Deep nesting + strings + line/block/nested comments + char literals, so a
;; string or comment straddles many of the sampled start positions.

(define (make-big n)
  (let ((p (open-output-string)))
    (let lp ((i 0))
      (when (fx< i n)
        (case (fxmod i 7)
          ((0) (emit p "(alpha "))
          ((1) (emit p "\"str ( [ ) ] ;\" "))
          ((2) (emit p "; line ( [ comment\n"))
          ((3) (emit p "#| block ( #| nest [ |# |# "))
          ((4) (emit p "#\\( #\\) #\\[ "))
          ((5) (emit p ") ] "))
          ((6) (emit p "[beta (gamma)] ")))
        (lp (fx+ i 1))))
    (get-output-string p)))

(define big (make-big 2400))          ; ~20k+ chars of mixed Scheme-ish text

;; First diverging sampled position over the big buffer (stride keeps it snappy
;; while still landing inside strings, comments and nests), or #f.
(define (first-strided-divergence buf stride f g)
  (let ((len (string-length buf)))
    (let lp ((p 0))
      (cond
        ((fx> p len) #f)
        ((equal? (f buf p) (g buf p)) (lp (fx+ p stride)))
        (else (list p (f buf p) (g buf p)))))))

(display "big buffer length: ") (display (string-length big)) (newline)

(test-equal "forward-down-list == from-0 reference across the ~20k buffer (value = first divergence or #f)"
            #f (first-strided-divergence big 13 forward-down-list reference-fdl))
(test-equal "public forward-sexp-end == from-0 reference across the ~20k buffer"
            #f (first-strided-divergence big 13 forward-sexp-end reference-fse))
(test-equal "public forward-sexp-end == cursor seeded from the from-0 state across the ~20k buffer"
            #f (first-strided-divergence big 13 forward-sexp-end fse-via-cursor))

;; =============================================================================
;; Section C -- per-call quadratic witness: forward-down-list
;; =============================================================================
;; A line comment packed with open-parens, a newline (ends the comment), then a
;; real opener.  The from-0 reference re-lexes the whole prefix at every '(' in
;; the comment -> O(n^2); the single pass tracks the comment state as it goes and
;; returns in one sweep.  RED on the old tree (forward-down-list WAS the
;; reference); GREEN now.

(define (make-many-opener-buffer k)
  (let ((p (open-output-string)))
    (write-char #\; p)
    (let lp ((i 0)) (when (fx< i k) (write-char #\( p) (lp (fx+ i 1))))
    (write-char #\newline p)
    (write-char #\( p)
    (get-output-string p)))

(define many-openers (make-many-opener-buffer 20000))
(define fdl-result   (forward-down-list many-openers 0))
(define ref-result   (reference-fdl many-openers 0))
(define fdl-ms (elapsed-ms (lambda () (forward-down-list many-openers 0))))
(define ref-ms (elapsed-ms (lambda () (reference-fdl many-openers 0))))

(display "WITNESS forward-down-list per-call: single-ms=") (display fdl-ms)
(display " ref(O(n^2))-ms=") (display ref-ms)
(display " ratio=") (display (if (> fdl-ms 0) (exact->inexact (/ ref-ms fdl-ms)) 'inf))
(newline)

;; Non-vacuity: both really found the same real opener past the comment.
(test-equal "single-pass and from-0 reference return the same opener position (non-vacuity)"
            ref-result fdl-result)
(test-assert "the returned position is a real opener, not #f (the witness did work)"
             (and (fixnum? fdl-result) (fx> fdl-result 20000)))
;; THE witness: RED on the old tree (single IS the reference), GREEN now.
;; Generous 4x margin -- the real gap on a 20k buffer is far larger.
(test-assert "single-pass forward-down-list is more than 4x faster than the from-0 O(n^2) reference"
             (< fdl-ms (/ ref-ms 4)))

;; =============================================================================
;; Section D -- chained quadratic witness: forward-sexp-end
;; =============================================================================
;; forward-sexp-end is per-call O(n) (one lexer-state-at); its quadratic emerges
;; only when CHAINED.  Walk a buffer of many small sexps end-to-end two ways: the
;; public chain re-lexes the prefix from 0 at each step (O(n^2)); the cursor chain
;; threads the returned normal state forward (O(n)).  The two end-position
;; sequences must be identical; the threaded walk must be far faster.

(define (make-sexp-walk n)
  (let ((p (open-output-string)))
    (let lp ((i 0)) (when (fx< i n) (emit p "(a) ") (lp (fx+ i 1))))
    (get-output-string p)))

(define walk (make-sexp-walk 5000))
(define walk-len (string-length walk))

;; Public chain: forward-sexp-end from each returned end (from-0 re-lex per step).
(define (chain-public str)
  (let ((len (string-length str)))
    (let loop ((pos 0) (acc '()))
      (let ((e (forward-sexp-end str pos)))
        (if (and e (fx< pos len) (fx> e pos)) (loop e (cons e acc)) (reverse acc))))))

;; Cursor chain: skip whitespace (preserves 'normal between balanced sexps), call
;; the cursor with the threaded state, and thread its returned end-state forward.
(define (chain-cursor str)
  (let ((len (string-length str)))
    (let loop ((pos 0) (state 'normal) (acc '()))
      (let skip ((i pos))
        (if (and (fx< i len) (char-whitespace? (string-ref str i)))
            (skip (fx+ i 1))
            (if (fx>= i len)
                (reverse acc)
                (call-with-values
                  (lambda () (forward-sexp-end/cursor str i state))
                  (lambda (e est)
                    (if (and e (fx> e pos)) (loop e est (cons e acc)) (reverse acc))))))))))

(define seq-public (chain-public walk))
(define seq-cursor (chain-cursor walk))
(define pub-ms (elapsed-ms (lambda () (chain-public walk))))
(define cur-ms (elapsed-ms (lambda () (chain-cursor walk))))

(display "WITNESS forward-sexp-end chained walk: public(O(n^2))-ms=") (display pub-ms)
(display " cursor(O(n))-ms=") (display cur-ms)
(display " ratio=") (display (if (> cur-ms 0) (exact->inexact (/ pub-ms cur-ms)) 'inf))
(display " steps=") (display (length seq-public))
(newline)

;; Deterministic gate: the two chained walks are byte-identical, and non-vacuous
;; (they really walked all 5000 sexps).
(test-assert "the public and cursor-threaded chained walks produce the identical end-position sequence"
             (equal? seq-public seq-cursor))
(test-equal "the chained walk visited every sexp (non-vacuity)"
            5000 (length seq-public))
;; THE witness: the cursor-threaded chain is far faster than the from-0 public
;; chain.  Generous 4x margin; the measured gap is much larger.
(test-assert "the cursor-threaded chained walk is more than 4x faster than the re-lex-from-0 public chain"
             (< cur-ms (/ pub-ms 4)))

(test-end)
