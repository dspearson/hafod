;;; test/test-comp-menu.ss -- The aligned-description proof for the completion
;;; menu, driven through the production grid renderer.  When candidates carry
;;; descriptions the grid pads every candidate to a common column so the
;;; descriptions line up beneath one another (the zsh/fish look), and it truncates
;;; an over-long description to the terminal width with an ellipsis.  Both
;;; behaviours are asserted here against render-completion-grid -- the renderer the
;;; live editor actually draws with -- rather than the exported-but-undrawn menu
;;; variant, so a green result reflects what the user sees.  Entirely PTY-free: the
;;; virtual-terminal harness folds the renderer's escape stream into a cell grid,
;;; so the suite opens no terminal and needs no platform gate.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor render) render-completion-grid)
        (chezscheme))

(test-begin "comp-menu")

;; ======================================================================
;; Helpers
;; ======================================================================

;; The 0-based index of the first occurrence of NEEDLE in HAY, or #f when absent.
;; A folded row is built from column 0, so an index into the row string is the
;; screen column of that glyph.
(define (substring-index hay needle)
  (let ([hl (string-length hay)]
        [nl (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nl) hl) #f]
        [(string=? (substring hay i (+ i nl)) needle) i]
        [else (loop (+ i 1))]))))

;; The screen column at which NEEDLE first appears anywhere in the folded grid,
;; or #f when no row carries it.  Descriptions are unique per row, so this reads
;; back the column each one starts at.
(define (needle-col scr needle)
  (let loop ([lines (vterm-lines scr)])
    (cond
      [(null? lines) #f]
      [(substring-index (car lines) needle) => (lambda (i) i)]
      [else (loop (cdr lines))])))

;; True when STR's last glyph is the U+2026 horizontal ellipsis truncate-display
;; appends when it drops the tail of an over-wide string.
(define (ends-with-ellipsis? str)
  (let ([n (string-length str)])
    (and (> n 0) (char=? (string-ref str (- n 1)) #\x2026))))

;; ======================================================================
;; (a) Aligned descriptions -- the alignment invariant on the live grid.
;; Three candidates of DIFFERING widths, each carrying a description, are rendered
;; on a wide terminal with no selection.  The grid pads every candidate to the
;; width of the widest before its description, so each description's first glyph
;; lands at one common column regardless of the candidate's own width.  A naive
;; renderer that placed a description right after its candidate would start the
;; three at three different columns, so the equal-column assertion fails on such a
;; tree -- it is non-vacuous.
;; ======================================================================

(let* ([entries '(("a"    () "alpha")
                  ("bbbb" () "beta")
                  ("cc"   () "gamma"))]
       [scr (render->screen 40
              (lambda (p) (render-completion-grid p entries -1 40 10)))]
       [col-alpha (needle-col scr "alpha")]
       [col-beta  (needle-col scr "beta")]
       [col-gamma (needle-col scr "gamma")])
  (test-assert "menu: every candidate's description is drawn into the grid"
    (and col-alpha col-beta col-gamma))
  (test-assert "menu: descriptions for mixed-width candidates share one start column"
    (and (= col-alpha col-beta) (= col-beta col-gamma))))

;; ======================================================================
;; (b) Over-long description truncation -- pins truncate-display.
;; On a narrow terminal a description wider than the remaining room is truncated
;; and closed with the U+2026 ellipsis.  The candidate carrying the long
;; description is the NARROWER of the two, so its own name is not truncated and
;; the ellipsis at the row's end is unambiguously the description's.  The row is
;; found by the description's leading word, so the assertion cannot pass on an
;; unrelated line.
;; ======================================================================

(let* ([long-desc "alpha beta gamma delta epsilon zeta eta theta"]
       [entries (list (list "aa"        '() long-desc)
                      (list "wwwwwwwww" '() "brief"))]
       [scr (render->screen 30
              (lambda (p) (render-completion-grid p entries -1 30 10)))]
       [desc-row (let loop ([lines (vterm-lines scr)])
                   (cond
                     [(null? lines) ""]
                     [(substring-index (car lines) "alpha") (car lines)]
                     [else (loop (cdr lines))]))])
  (test-assert "menu: the long description's row is present in the grid"
    (substring-index desc-row "alpha"))
  (test-assert "menu: an over-long description is truncated with the U+2026 ellipsis"
    (ends-with-ellipsis? desc-row)))

(test-end)
