;;; test/test-transient-prompt.ss -- The opt-in transient prompt.  With
;;; transient-prompt? on, once a command is accepted the just-spent prompt is
;;; repainted in a minimal form -- its exit-coloured input glyph alone, the
;;; multi-line info segments (path/git/versions) dropped -- before the next prompt
;;; draws, keeping scrollback lean.  Off (the default) the spent prompt stays
;;; fully rendered exactly as shipped.
;;;
;;; The proof DRIVES the real read-expression over an open-input-string (end of
;;; input submits, so finish! -- where the collapse lives -- actually runs) and
;;; folds its escape stream into the (test vterm) cell grid.  The prompt handed to
;;; read-expression is the very string interactive mode builds from the segment
;;; model -- an info left segment plus the exit-coloured input glyph as the line
;;; segment -- so the collapse is exercised end to end against the shipped prompt
;;; shape.  Entirely PTY-free: a string port and the virtual-terminal grid, so it
;;; opens no terminal and runs identically on every platform.
;;;
;;; The rows:
;;;   (a) collapsed on -- the info line is gone from scrollback and the spent
;;;       prompt is the input glyph plus the typed command alone, which survives
;;;       verbatim;
;;;   (b) off by default -- the same drive with the toggle unset leaves the info
;;;       line on its own row above the command (the non-vacuity anchor), and the
;;;       transient drive occupies strictly fewer rows than it;
;;;   (c) exit colour -- the collapsed glyph carries whatever colour the live
;;;       prompt already had (the PRIOR command's status: the just-submitted
;;;       command has not run at finish! time), green after a success and red
;;;       after a non-zero exit, read back off the cell under a pinned verdict.
;;;
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor editor) read-expression transient-prompt?)
        ;; The segment model that builds the live prompt string: an info left
        ;; segment plus the exit-coloured input glyph as the line segment, rendered
        ;; to the exact "info...\n<glyph> " shape read-expression is handed.
        (only (hafod interactive)
              clear-prompt-segments! register-prompt-segment!
              make-current-prompt-ctx render-prompt-segments
              prompt-char-segment last-status)
        (only (hafod terminal-caps) glyph-tier-override)
        (only (hafod environment) with-env*)
        (chezscheme))

(test-begin "transient-prompt")

;; ======================================================================
;; Helpers
;; ======================================================================

;; Naive substring search (the runner carries no string-contains).
(define (contains-substring? hay needle)
  (let ([hl (string-length hay)]
        [nl (string-length needle)])
    (let outer ([i 0])
      (cond
        [(> (+ i nl) hl) #f]
        [(string=? (substring hay i (+ i nl)) needle) #t]
        [else (outer (+ i 1))]))))

;; Does ANY row of the folded grid carry NEEDLE?  Folds vterm-lines through
;; contains-substring? so the info line (or the command) is findable wherever it
;; landed in the grid.
(define (any-line-has? scr needle)
  (let loop ([lines (vterm-lines scr)])
    (cond
      [(null? lines) #f]
      [(contains-substring? (car lines) needle) #t]
      [else (loop (cdr lines))])))

;; The path/git info line the transient collapse must drop, and the command that
;; must survive in scrollback.  Neither is a substring of the other, so their
;; presence/absence tells the two apart on the grid.
(define info-marker "~/project main")
(define cmd "ls -la")

;; The heavy right-angle input glyph (U+276F) the emoji tier renders.
(define glyph "\x276f;")

;; Build the two-line informative prompt string interactive mode hands
;; read-expression: an info left segment plus the exit-coloured input glyph as the
;; line segment, rendered under a forced capability verdict and the emoji glyph
;; tier so the glyph and its exit colour are pinned dev==CI.  EXIT is the PRIOR
;; command's status the input glyph colours by -- green on 0, red otherwise --
;; captured at build time exactly as the live prompt captures it.
(define (build-prompt exit)
  (parameterize ([assume-terminal-caps 'on]
                 [glyph-tier-override 'emoji]
                 [last-status exit])
    (clear-prompt-segments!)
    (register-prompt-segment! 'left (lambda (ctx) info-marker))
    (register-prompt-segment! 'line prompt-char-segment)
    (let ([p (open-output-string)])
      (render-prompt-segments p (make-current-prompt-ctx))
      (get-output-string p))))

;; Drive read-expression onto a folded COLS-wide screen: type KEYS (end of input
;; submits, so finish! and its collapse run) at a given transient-prompt? setting.
;; $LS_COLORS is pinned and render->screen forces the capability verdict on, so a
;; colour a cell reads back is deterministic rather than an artifact of the
;; developer's or CI's environment.
(define (drive prompt keys cols transient?)
  (with-env* '(("LS_COLORS" . ""))
    (lambda ()
      (render->screen cols
        (lambda (sp)
          (parameterize ([transient-prompt? transient?])
            (read-expression prompt (open-input-string keys) sp)))))))

(define prompt-ok  (build-prompt 0))    ; success -> green input glyph
(define prompt-bad (build-prompt 1))    ; failure -> red input glyph

;; ======================================================================
;; (a) Collapsed on -- a spent prompt drops its info segments, keeps the command
;; ======================================================================

(define on-scr (drive prompt-ok cmd 80 #t))

(test-assert "collapsed: the path/git info line is gone from scrollback"
  (not (any-line-has? on-scr info-marker)))
(test-assert "collapsed: the typed command survives in scrollback verbatim"
  (any-line-has? on-scr cmd))
(test-equal "collapsed: the spent prompt is the input glyph plus the command alone"
  (string-append glyph " " cmd) (vterm-row-text on-scr 0))
(test-equal "collapsed: the surviving glyph is the heavy right-angle input glyph"
  #\x276f (cell-glyph (vterm-cell on-scr 0 0)))

;; ======================================================================
;; (b) Off by default -- the shipped full prompt is untouched (non-vacuity anchor)
;; ======================================================================

(define off-scr (drive prompt-ok cmd 80 #f))

(test-equal "off by default: the info line still occupies its own row"
  info-marker (vterm-row-text off-scr 0))
(test-assert "off by default: the command sits on the row below the info line"
  (contains-substring? (vterm-row-text off-scr 1) cmd))
(test-assert "collapsed vs off: the transient prompt occupies strictly fewer rows"
  (< (vterm-rows on-scr) (vterm-rows off-scr)))

;; ======================================================================
;; (c) Exit colour -- the collapsed glyph carries the prior status' colour
;; ======================================================================

;; The success drive above coloured the glyph green (256 index 2).  A fixed-colour
;; glyph would fail one of this pair.
(test-equal "exit colour: the collapsed glyph is green (256 index 2) after a success"
  2 (cell-fg (vterm-cell on-scr 0 0)))

(define bad-scr (drive prompt-bad cmd 80 #t))
(test-equal "exit colour: the collapsed glyph is red (256 index 1) after a non-zero exit"
  1 (cell-fg (vterm-cell bad-scr 0 0)))
(test-assert "exit colour: the failure drive still collapses the info line away"
  (not (any-line-has? bad-scr info-marker)))

(test-end)
