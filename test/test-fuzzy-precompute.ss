;;; test-fuzzy-precompute.ss -- Byte-identity + per-keystroke-work witnesses for
;;; the (hafod fuzzy) precompute cache and the reused, re-zeroed DP scratch.
;;;
;;; The refactor hoists the two query-independent build-loop artefacts (the
;;; case-insensitive fold T_ci and the class-based bonus array B) out of the
;;; per-keystroke path and behind a default-off cache, and reuses a re-zeroed
;;; module-level H/C scratch instead of allocating a fresh matrix per score.
;;; Neither may move a single score or highlight position, so this suite proves
;;; four things:
;;;
;;;   1. Golden anchors -- concrete (fuzzy-score ...) values captured from the
;;;      pre-refactor build across the case-sensitive, case-insensitive and
;;;      accented paths. Equality to those values IS the byte-identity gate.
;;;   2. Cache-on == cache-off -- filter-search-pattern/positions returns the
;;;      identical ranking + positions whether the cache is defaulted (#f, the
;;;      inline build path every non-finder caller takes) or parameterised to a
;;;      fresh eq-hashtable, over case-sensitive / case-insensitive / accented
;;;      queries. A non-vacuity control proves the observer stays silent on the
;;;      default path.
;;;   3. Build-count -- with a routed observer, each candidate's (T_ci . B) is
;;;      built at most once across two successive keystrokes (a per-keystroke
;;;      rebuild would fire on every keystroke, reddening this).
;;;   4. Scratch reuse -- scoring a long, wide-DP candidate then short candidates
;;;      in the same session yields each short's exact isolated (score .
;;;      positions): the shared scratch is re-zeroed, so no stale value leaks in.
;;;
;;; A (real-time) timing pair (cache-off rebuild-per-keystroke vs cache-on) is
;;; emitted unconditionally on a WITNESS line for the SUMMARY. The timing assert
;;; is deliberately generous -- the deterministic build-count and byte-identity
;;; asserts are the hard gates. British-English throughout.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (chezscheme)
        (only (hafod fuzzy)
              fuzzy-score fuzzy-match filter-search-pattern/positions
              fuzzy-precompute-cache fuzzy-precompute-observer))

(test-begin "fuzzy-precompute")

;; In-tree timing idiom: elapsed real time in ms around a thunk.
(define (elapsed-ms thunk)
  (let ([t0 (real-time)]) (thunk) (- (real-time) t0)))

;; Fastest of n runs -- damps scheduling noise on the timing witness.
(define (best-of n thunk)
  (let lp ([k 0] [best #f])
    (if (fx= k n) best
        (let ([ms (elapsed-ms thunk)])
          (lp (fx1+ k) (if (or (not best) (< ms best)) ms best))))))

;; Accented strings built from explicit code points so the file's own encoding
;; cannot skew the accented (diacritic-fold) path. U+00E9 = e-acute.
(define cafe (string #\c #\a #\f (integer->char #xE9)))    ; "café"

;; ===========================================================================
;; 1. Golden anchors -- byte-identity to the pre-refactor build
;; ===========================================================================
;; Captured live from HEAD before the refactor. They span the case-sensitive
;; ("Fz", "SE"), case-insensitive ("sexp", "ab") and accented ("caf"/"café")
;; paths. Equality to these exact values is the immediate byte-identity gate.

(test-equal "golden: (fuzzy-score \"sexp\" \"sexp-tracker\") = 114 (ci)"
  114 (fuzzy-score "sexp" "sexp-tracker"))
(test-equal "golden: (fuzzy-score \"ab\" \"aXbXcX\") = 49 (ci, scattered)"
  49 (fuzzy-score "ab" "aXbXcX"))
(test-equal "golden: (fuzzy-score \"Fz\" \"src/hafod/Fuzzy.ss\") = 47 (cs)"
  47 (fuzzy-score "Fz" "src/hafod/Fuzzy.ss"))
(test-equal "golden: (fuzzy-score \"caf\" \"café\") = 88 (accented fold)"
  88 (fuzzy-score "caf" cafe))
(test-equal "golden: (fuzzy-score \"SE\" \"SexpTracker\") = #f (cs, no match)"
  #f (fuzzy-score "SE" "SexpTracker"))

;; Full (score . positions) anchors -- pin the highlight positions too, so a
;; backtrace drift (not merely a score drift) would also redden.
(test-equal "golden positions: (fuzzy-match \"sexp\" \"sexp-tracker\") = (114 0 1 2 3)"
  '(114 0 1 2 3) (fuzzy-match "sexp" "sexp-tracker"))
(test-equal "golden positions: (fuzzy-match \"fz\" \"fuzzy\") = (49 0 2)"
  '(49 0 2) (fuzzy-match "fz" "fuzzy"))

;; ===========================================================================
;; 2. Cache-on == cache-off equivalence (the ranking byte-identity oracle)
;; ===========================================================================

(define corpus
  (list "fuzzy" "finder" "future" "phase" "fizz" "FooBarFz"
        "src/hafod/fuzzy.ss" "src/hafod/Fuzzy.ss" cafe "Café-Bar"
        "a.b.c" "alpha-beta" "SexpTracker" "sexp-tracker" "no-match-here"))

;; Rank the corpus with the cache defaulted (#f -- today's inline path) or a
;; fresh eq-hashtable installed. Same candidate order + positions both ways.
(define (rank q with-cache?)
  (if with-cache?
      (parameterize ([fuzzy-precompute-cache (make-eq-hashtable)])
        (filter-search-pattern/positions q corpus))
      (filter-search-pattern/positions q corpus)))

;; A query set spanning smart-case (lower vs mixed), accents and the empty query.
(define equiv-queries (list "fz" "Fz" "sexp" "SE" cafe "a.b" "" "src/ha" "fuzzy"))

(test-assert "cache-on ranking + positions equal cache-off over cs/ci/accent queries"
  (for-all (lambda (q) (equal? (rank q #f) (rank q #t))) equiv-queries))

;; Non-vacuity control: with no cache installed the observer must never fire --
;; the default path is exactly today's inline build, untouched.
(test-assert "with cache defaulted (#f) the observer never fires (default path unchanged)"
  (let ([fired 0])
    (parameterize ([fuzzy-precompute-observer (lambda () (set! fired (fx+ fired 1)))])
      (filter-search-pattern/positions "f" corpus)
      (fx= fired 0))))

;; ===========================================================================
;; 3. Build-count witness -- each (T_ci . B) built at most once across keystrokes
;; ===========================================================================
;; A fresh cache + an observer thunk that counts builds. Over two successive
;; keystrokes ("f" then "fu") every candidate is scored on each keystroke, so a
;; per-keystroke rebuild would fire 2*N times; the cache fires exactly N (once
;; per distinct candidate). Deterministic -- this is a hard gate.

(define bc-cands (list "fuzzy" "finder" "future" "fizz" "affix"))  ; all len > 2, all reach V2

(test-equal "each candidate's (T_ci . B) is built exactly once across two keystrokes"
  (length bc-cands)
  (let ([builds 0])
    (parameterize ([fuzzy-precompute-cache (make-eq-hashtable)]
                   [fuzzy-precompute-observer (lambda () (set! builds (fx+ builds 1)))])
      (filter-search-pattern/positions "f"  bc-cands)   ; keystroke 1: builds N
      (filter-search-pattern/positions "fu" bc-cands)   ; keystroke 2: all cache hits
      builds)))

(test-assert "the build observer actually fired (witness is non-vacuous)"
  (let ([builds 0])
    (parameterize ([fuzzy-precompute-cache (make-eq-hashtable)]
                   [fuzzy-precompute-observer (lambda () (set! builds (fx+ builds 1)))])
      (filter-search-pattern/positions "f" bc-cands)
      (fx> builds 0))))

;; ===========================================================================
;; 4. Scratch-reuse witness -- long-before-short yields the short's exact result
;; ===========================================================================
;; The DP reuses a module-level H/C scratch grown to the largest M*width ever
;; scored and re-zeroed before each score. Scoring a long, wide-DP candidate
;; grows and fills that scratch; a subsequently scored short candidate reuses it
;; and MUST still produce its exact isolated (score . positions).

(let* ([pat "ab"]
       [short "aXbXcX"]
       ;; long: pattern 'a' matches at 0, 'b' at 400 -> width 401, M=2, a wide DP
       ;; that grows and fills the shared scratch well past the short candidate's
       ;; own [0, M*width) region.
       [long (string-append (make-string 400 #\a) "b")]
       [iso (fuzzy-match pat short)])          ; isolated baseline on the current scratch
  (fuzzy-match pat long)                        ; grow + fill the shared scratch
  (test-equal "short 'ab'/'aXbXcX' after a long candidate: exact golden (49 0 2)"
    '(49 0 2) (fuzzy-match pat short))
  (test-equal "short-after-long equals short-in-isolation (re-zeroed scratch)"
    iso (fuzzy-match pat short)))

;; A batch of differently-shaped short candidates, each re-scored after a long
;; pollutant, all reproducing their isolated results -- a broader re-zero guard.
(let* ([cases (list (cons "ab" "aXbXcX")
                    (cons "sexp" "sexp-tracker")
                    (cons "abc" "aWWbWWc")
                    (cons "fz" "fuzzy")
                    (cons "caf" cafe))]
       [iso (map (lambda (pc) (fuzzy-match (car pc) (cdr pc))) cases)]
       [long (string-append (make-string 500 #\a) "bcz")])
  (fuzzy-match "abc" long)                       ; pollute the shared scratch
  (test-assert "a batch of short candidates all score identically after scratch pollution"
    (for-all (lambda (pc want) (equal? want (fuzzy-match (car pc) (cdr pc)))) cases iso)))

;; ===========================================================================
;; 5. Timing pair -- cache-off (rebuild per keystroke) vs cache-on
;; ===========================================================================
;; Front-clustered corpus: a "fuzzy" prefix plus a long tail. The pattern chars
;; match at the front (tiny DP width) while the candidate is long (O(N)
;; precompute), so the per-keystroke rebuild the cache eliminates dominates and
;; the win is visible. The pair is emitted for the SUMMARY; the assert is
;; generous (the build-count above is the hard rebuild-per-keystroke gate).

(define big-corpus
  (let lp ([i 0] [acc '()])
    (if (fx= i 3000) acc
        (lp (fx1+ i)
            (cons (string-append "fuzzy" (make-string 120 #\z) (number->string i)) acc)))))

(define keystrokes '("f" "fu" "fuz" "fuzz" "fuzzy" "fuzzyz" "fuzzyzz"))

(define (run-cache-off)
  (for-each (lambda (q) (filter-search-pattern/positions q big-corpus)) keystrokes))

(define (run-cache-on)
  (parameterize ([fuzzy-precompute-cache (make-eq-hashtable)])
    (for-each (lambda (q) (filter-search-pattern/positions q big-corpus)) keystrokes)))

;; Warm both paths (pages, code, GC) before timing.
(run-cache-off)
(run-cache-on)
(define off-ms (best-of 3 run-cache-off))
(define on-ms  (best-of 3 run-cache-on))

(display "WITNESS fuzzy-precompute: cache-off-ms=") (display off-ms)
(display " cache-on-ms=") (display on-ms)
(display " ratio=")
(display (if (> on-ms 0) (exact->inexact (/ off-ms on-ms)) 'inf))
(display " (") (display (length big-corpus)) (display " candidates x ")
(display (length keystrokes)) (display " keystrokes)")
(newline)

;; Positive control: both paths produce the identical ranking on the last
;; keystroke -- the cache does the whole job, only skipping the rebuild.
(test-assert "cache-off and cache-on rank the big corpus identically (fast path is not doing less)"
  (equal? (filter-search-pattern/positions "fuzzy" big-corpus)
          (parameterize ([fuzzy-precompute-cache (make-eq-hashtable)])
            (filter-search-pattern/positions "fuzzy" big-corpus))))

;; Generous timing gate: the cache path must not be materially slower than the
;; rebuild-every-keystroke path. Measured ~1.2x faster on the flake Chez; the
;; slack absorbs CI noise without letting a gross regression through.
(test-assert "cache-on is not slower than cache-off (rebuild-per-keystroke eliminated)"
  (<= on-ms (+ off-ms (max 25 (quotient off-ms 3)))))

(test-end)
