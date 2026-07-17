(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (chezscheme)
        (hafod finder)
        (only (hafod fuzzy) fuzzy-precompute-cache fuzzy-precompute-observer))

(test-begin "finder-vterm-rank")

;; ======================================================================
;; The finder session now shares one fuzzy precompute cache
;; across keystrokes.  These witnesses prove the wiring is byte-identical
;; at both the DATA level (finder-state-filtered) and the RENDERED level
;; (the PTY-free (test vterm) cell grid), and that the shared cache is
;; actually HIT across keystrokes (the reuse the finder was wired for).
;;
;; The finder loop is only reachable through a real terminal, so these
;; drive the finder's per-keystroke entry point (query-insert! -> refilter!
;; -> filter-search-pattern/positions) directly, installing a fresh session
;; cache with the SAME parameterize run-finder* now wraps around the loop.
;; ======================================================================

;; A finder-state at the empty query, mirroring run-finder*'s init and the
;; make-narrow-state helper in test-finder.ss.  make-finder-state stays a 14-arg
;; positional call -- no field was added for the cache.  colour? gates whether
;; render-finder! emits the SGR/highlight bytes the rendered oracle inspects.
(define (make-rank-state items-vec colour?)
  (make-finder-state
    items-vec
    (list->vector (map (lambda (s) (cons s '())) (vector->list items-vec)))
    "" 0 0 0                          ; query cursor selected scroll-offset
    24 80                             ; rows cols
    (vector-length items-vec) "> "    ; total-count prompt
    items-vec                         ; display-items (single-line: flatten to self)
    #f #f colour?))                   ; colorize? show-numbers? colour?

;; Type each character through the real query-insert! entry point, so refilter!
;; runs with the true pre-mutation query and takes the finder's narrowing path
;; per keystroke -- exactly the hot path the session cache serves.
(define (type-query! state s)
  (let ([n (string-length s)])
    (let lp ([i 0])
      (when (fx< i n)
        (query-insert! state (string-ref s i))
        (lp (fx1+ i))))))

;; Mixed corpus spanning smart-case (a capital query flips to case-sensitive)
;; and an accented candidate (café folds to cafe on the case-insensitive path),
;; so a mis-cached normalised text T would diverge on at least one query.
(define rank-corpus
  (vector "fuzzy-finder"
          "src/hafod/fuzzy.ss"
          "future"
          "café-mode"
          "Fuzzy.ss"
          "sexp-tracker"
          "phase"
          "FINDER"))

;; ======================================================================
;; Data-level byte-identity: finder-state-filtered per keystroke
;; ======================================================================

;; Drive `query` character by character on a fresh state and collect the
;; finder-state-filtered snapshot after EACH keystroke.  cache? installs a fresh
;; session cache around the whole drive (what run-finder* now does); otherwise the
;; default #f path (today's pre-cache behaviour).  refilter! replaces the filtered
;; vector wholesale each keystroke, so the per-keystroke snapshots never alias.
(define (drive-snapshots query cache?)
  (let ([state (make-rank-state rank-corpus #f)]
        [snaps '()])
    (define (run)
      (let ([n (string-length query)])
        (let lp ([i 0])
          (when (fx< i n)
            (query-insert! state (string-ref query i))
            (set! snaps (cons (vector->list (finder-state-filtered state)) snaps))
            (lp (fx1+ i))))))
    (if cache?
        (parameterize ([fuzzy-precompute-cache (make-eq-hashtable)]) (run))
        (run))
    (reverse snaps)))

;; For every query, the per-keystroke filtered vectors (candidates + positions +
;; order) are equal? with the session cache installed versus the default cache-off
;; path.  Fz / SE exercise the case-sensitive path (T = text verbatim, NOT the
;; cached fold); caf exercises the accented case-insensitive fold.
(for-each
  (lambda (q)
    (test-equal (string-append "finder-state-filtered byte-identical per keystroke, cache-on == cache-off: " q)
      (drive-snapshots q #f)
      (drive-snapshots q #t)))
  '("f" "fu" "fz" "Fz" "caf" "se" "SE" "future" "z"))

;; Teeth: the equivalence above is not passing on empty results.
(test-assert "the equivalence corpus actually produces matches (non-vacuous)"
  (let ([state (make-rank-state rank-corpus #f)])
    (type-query! state "fu")
    (fx> (vector-length (finder-state-filtered state)) 0)))

;; ======================================================================
;; Rendered-level byte-identity: the (test vterm) cell grid
;; ======================================================================

;; Capture every byte render-finder! would emit for `state`, with no escapes
;; leaking to the real console.
(define (render-capture state)
  (let ([sp (open-output-string)])
    (parameterize ([console-output-port sp])
      (render-finder! state))
    (get-output-string sp)))

;; Fold a captured byte stream into a fresh vterm grid.
(define (grid-of cols bytes)
  (let ([vt (make-vterm cols)])
    (vterm-feed! vt bytes)
    vt))

;; Two cells agree on glyph AND every pen attribute (fg/bg/bold/dim/underline/
;; reverse) -- so a divergence in ranking OR in match-highlight position would
;; show up here.
(define (cells-equal? a b)
  (or (and (not a) (not b))
      (and a b
           (char=? (cell-glyph a) (cell-glyph b))
           (equal? (cell-fg a) (cell-fg b))
           (equal? (cell-bg a) (cell-bg b))
           (eq? (cell-bold? a) (cell-bold? b))
           (eq? (cell-dim? a) (cell-dim? b))
           (eq? (cell-underline? a) (cell-underline? b))
           (eq? (cell-reverse? a) (cell-reverse? b)))))

(define (grids-equal? a b)
  (and (= (vterm-rows a) (vterm-rows b))
       (= (vterm-cols a) (vterm-cols b))
       (let row-lp ([r 0])
         (or (fx>= r (vterm-rows a))
             (and (string=? (vterm-row-text a r) (vterm-row-text b r))
                  (let col-lp ([c 0])
                    (or (fx>= c (vterm-cols a))
                        (and (cells-equal? (vterm-cell a r c) (vterm-cell b r c))
                             (col-lp (fx1+ c)))))
                  (row-lp (fx1+ r)))))))

;; True iff some cell carries a bold highlight -- proving the render is
;; non-vacuous (the query actually produced highlighted matches to compare).
(define (grid-has-bold? vt)
  (let ([rows (vterm-rows vt)] [cols (vterm-cols vt)])
    (let row-lp ([r 0])
      (and (fx< r rows)
           (or (let col-lp ([c 0])
                 (and (fx< c cols)
                      (let ([cell (vterm-cell vt r c)])
                        (or (and cell (cell-bold? cell))
                            (col-lp (fx1+ c))))))
               (row-lp (fx1+ r)))))))

(let* ([cols 80]
       [q "fu"]
       ;; colour? = #t so render-finder! emits the SGR/highlight bytes; the two
       ;; states are byte-identical inputs to render except for the ambient cache.
       [off-grid (let ([st (make-rank-state rank-corpus #t)])
                   (type-query! st q)
                   (grid-of cols (render-capture st)))]
       [on-grid  (let ([st (make-rank-state rank-corpus #t)])
                   (parameterize ([fuzzy-precompute-cache (make-eq-hashtable)])
                     (type-query! st q))
                   (grid-of cols (render-capture st)))])
  (test-assert "rendered finder grid is non-vacuous (a bold match highlight is present)"
    (grid-has-bold? on-grid))
  (test-assert "rendered finder grid is byte-identical cache-on == cache-off (glyph + SGR per cell)"
    (grids-equal? off-grid on-grid)))

;; ======================================================================
;; The shared cache is HIT across keystrokes (the reuse it was wired for)
;; ======================================================================

;; Under ONE session cache (run-finder*'s wiring), the first keystroke populates
;; each survivor's precompute; a second keystroke that narrows those survivors
;; rebuilds NOTHING -- the observer fires zero extra times.  A per-keystroke-fresh
;; cache (the rejected anti-pattern) would rebuild every survivor again.
(let ([builds 0] [after-1 0] [after-2 0]
      [state (make-rank-state rank-corpus #f)])
  (parameterize ([fuzzy-precompute-cache (make-eq-hashtable)]
                 [fuzzy-precompute-observer (lambda () (set! builds (fx1+ builds)))])
    (query-insert! state #\f)          ; keystroke 1: builds each survivor once
    (set! after-1 builds)
    (query-insert! state #\u)          ; keystroke 2: narrows survivors, all cached
    (set! after-2 builds))
  (test-equal "session cache: the second keystroke rebuilds no candidate (full reuse across keystrokes)"
    after-1 after-2))

;; The same drive, counted whole: a session cache builds strictly fewer times than
;; a fresh cache re-installed each keystroke -- the finder wiring's measurable win.
(define (session-builds query)
  (let ([builds 0]
        [state (make-rank-state rank-corpus #f)])
    (parameterize ([fuzzy-precompute-cache (make-eq-hashtable)]
                   [fuzzy-precompute-observer (lambda () (set! builds (fx1+ builds)))])
      (type-query! state query))
    builds))

(define (per-keystroke-builds query)
  (let ([builds 0]
        [state (make-rank-state rank-corpus #f)])
    (parameterize ([fuzzy-precompute-observer (lambda () (set! builds (fx1+ builds)))])
      (let ([n (string-length query)])
        (let lp ([i 0])
          (when (fx< i n)
            (parameterize ([fuzzy-precompute-cache (make-eq-hashtable)])
              (query-insert! state (string-ref query i)))
            (lp (fx1+ i))))))
    builds))

(test-assert "session cache builds strictly fewer than a per-keystroke cache (reuse is real)"
  (fx< (session-builds "fu") (per-keystroke-builds "fu")))

;; Record the measured pair for the log (non-asserting).
(let ([s (session-builds "fu")]
      [p (per-keystroke-builds "fu")])
  (display "WITNESS finder-cache: session-builds=") (display s)
  (display " per-keystroke-builds=") (display p)
  (display " (query \"fu\" over ") (display (vector-length rank-corpus))
  (display " candidates)") (newline))

;; ======================================================================
;; Supplementary (test vterm) row read-back: the ranked order renders as
;; the ordered candidate rows, and is the same order with the cache on.
;; ======================================================================

;; The finder draws bottom-up: best match nearest the prompt (row rows-2),
;; going up for later matches.  Read the candidate rows back as text and assert
;; the visible sequence is identical cache-on vs cache-off -- a direct read of
;; the RANKED ORDER through the grid, not just the per-cell attributes above.
(define (candidate-rows cols bytes rows)
  (let ([vt (grid-of cols bytes)])
    ;; rows-2 down to row 1 (row 0 unused here); collect non-empty rows top-most first.
    (let lp ([r 1] [acc '()])
      (if (fx> r (fx- rows 2))
          (reverse acc)
          (let ([txt (vterm-row-text vt r)])
            (lp (fx1+ r) (if (string=? txt "") acc (cons txt acc))))))))

(test-equal "the (test vterm) candidate-row order is identical cache-on == cache-off"
  (let ([st (make-rank-state rank-corpus #t)])
    (type-query! st "f")
    (candidate-rows 80 (render-capture st) 24))
  (let ([st (make-rank-state rank-corpus #t)])
    (parameterize ([fuzzy-precompute-cache (make-eq-hashtable)])
      (type-query! st "f"))
    (candidate-rows 80 (render-capture st) 24)))

(test-end)
