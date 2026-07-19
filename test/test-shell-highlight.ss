;;; test/test-shell-highlight.ss -- Per-cell colour assertions for the live
;;; shell-line highlighter, folded through the virtual terminal.
;;;
;;; The renderer now colours a shell-shaped command line by role: the command
;;; head green when it resolves on PATH / builtin / alias and red when it does
;;; not, quoted strings yellow, options/flags cyan, redirections magenta, and a
;;; non-existent path argument red + underlined.  This suite drives the genuine
;;; render-line / render-line/suggestion into the (test vterm) screen model via
;;; render->screen (which forces the colour verdict on so a plain string port
;;; emits its full escape stream) and reads the 256-colour foreground index and
;;; the underline attribute back off the exact cells the glyphs landed on.
;;;
;;; Classification is made deterministic by seeding one fake PATH command and one
;;; alias, so the suite never depends on what happens to sit on the host PATH.
;;; Every colour equality is paired with a control -- a lone bare word that stays
;;; the default foreground, a genuine Scheme form that keeps its rainbow paren
;;; colour, and the highlighter switched off -- so no assertion is vacuous.
;;;
;;; Entirely PTY-free: it opens no terminal and needs no platform gate.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor render)
              render-line render-line/suggestion
              shell-highlight? shell-highlight-paths?)
        (only (hafod shell classifier)
              rebuild-path-cache! path-cache alias-set!)
        (hafod editor gap-buffer)
        (chezscheme))

;; A gap buffer holding TEXT with the cursor left at end-of-buffer.
(define (buffer-from text)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb text)
    gb))

;; Render TEXT through render-line into a folded screen at 80 columns behind the
;; "> " prompt, so text column N lands at screen column N + 2.
(define (screen-of text)
  (let ([gb (buffer-from text)])
    (render->screen 80 (lambda (p) (render-line p "> " gb 0 80)))))

;; Seed deterministic classification: `myecho` resolves as a PATH command, `myll`
;; is an alias expanding to it (so an aliased head classifies shell and resolves),
;; while `zzznope` stays genuinely unknown.  `cd` is a builtin already.
(rebuild-path-cache!)
(hashtable-set! (path-cache) "myecho" #t)
(alias-set! "myll" "myecho")

(test-begin "shell-highlight")

;; ======================================================================
;; (1) Resolvable heads are green (256-index 2): a PATH command, a builtin, and
;; an alias whose expansion resolves.  A trailing flag is cyan (6).
;; ======================================================================

(let ([scr (screen-of "myecho -x")])
  (test-equal "head: a PATH command head glyph is m"
    #\m (cell-glyph (vterm-cell scr 0 2)))
  (test-equal "head: a resolvable PATH head is green (index 2)"
    2 (cell-fg (vterm-cell scr 0 2)))
  (test-equal "flag: an option flag glyph is -"
    #\- (cell-glyph (vterm-cell scr 0 9)))
  (test-equal "flag: an option flag is cyan (index 6)"
    6 (cell-fg (vterm-cell scr 0 9))))

(let ([scr (screen-of "cd foo")])
  (test-equal "head: a builtin head glyph is c"
    #\c (cell-glyph (vterm-cell scr 0 2)))
  (test-equal "head: a builtin head is green (index 2)"
    2 (cell-fg (vterm-cell scr 0 2))))

(let ([scr (screen-of "myll foo")])
  (test-equal "head: an alias head glyph is m"
    #\m (cell-glyph (vterm-cell scr 0 2)))
  (test-equal "head: an alias head resolves green (index 2)"
    2 (cell-fg (vterm-cell scr 0 2))))

;; ======================================================================
;; (2) An unresolved, shell-shaped head is red (index 1) -- the unknown-head case
;; classifier hides by sending an unknown head to scheme mode.  The flag beside
;; it is still cyan, proving the whole line is highlighted, not just the head.
;; ======================================================================

(let ([scr (screen-of "zzznope --x")])
  (test-equal "mark: an unknown shell-shaped head glyph is z"
    #\z (cell-glyph (vterm-cell scr 0 2)))
  (test-equal "mark: an unknown head is red (index 1)"
    1 (cell-fg (vterm-cell scr 0 2)))
  (test-equal "mark: the flag beside an unknown head is still cyan (index 6)"
    6 (cell-fg (vterm-cell scr 0 10))))

;; ======================================================================
;; (3) A quoted string is yellow (3) and a redirection operator is magenta (5).
;; ======================================================================

(let ([scr (screen-of "myecho \"hi\"")])
  (test-equal "string: the opening quote is yellow (index 3)"
    3 (cell-fg (vterm-cell scr 0 9)))
  (test-equal "string: a character inside the quotes is yellow too"
    3 (cell-fg (vterm-cell scr 0 10))))

(let ([scr (screen-of "myecho >out")])
  (test-equal "redir: the redirection glyph is >"
    #\> (cell-glyph (vterm-cell scr 0 9)))
  (test-equal "redir: a redirection operator is magenta (index 5)"
    5 (cell-fg (vterm-cell scr 0 9))))

;; ======================================================================
;; (4) A path-shaped argument is flagged red + underlined only when it does NOT
;; exist; an existing path stays the default foreground with no underline.  This
;; is the one filesystem-touching branch, so the discrimination is the proof.
;; ======================================================================

(let ([scr (screen-of "myecho /no/such/zz")])
  (test-equal "path: a missing path argument glyph is /"
    #\/ (cell-glyph (vterm-cell scr 0 9)))
  (test-equal "path: a missing path argument is red (index 1)"
    1 (cell-fg (vterm-cell scr 0 9)))
  (test-assert "path: a missing path argument is underlined"
    (cell-underline? (vterm-cell scr 0 9))))

(let ([scr (screen-of "myecho /")])
  (test-equal "path: an existing path argument keeps the default foreground"
    'default (cell-fg (vterm-cell scr 0 9)))
  (test-assert "path: an existing path argument is not underlined"
    (not (cell-underline? (vterm-cell scr 0 9)))))

;; ======================================================================
;; (5) Controls -- the shape gate leaves genuine Scheme alone.  A LONE unknown
;; word is not shell-shaped, so it is never reddened; a parenthesised form keeps
;; its rainbow paren colour (a 24-bit triple, not a 256-index), so the shell
;; colouriser did not run.
;; ======================================================================

(let ([scr (screen-of "zzznope")])
  (test-equal "control: a lone unknown word glyph is z"
    #\z (cell-glyph (vterm-cell scr 0 2)))
  (test-equal "control: a lone unknown word is NOT reddened (default foreground)"
    'default (cell-fg (vterm-cell scr 0 2))))

(let ([scr (screen-of "(+ 1 2)")])
  (test-equal "control: a Scheme form's open paren keeps its rainbow depth-0 colour"
    '(255 153 94) (cell-fg (vterm-cell scr 0 2))))

;; ======================================================================
;; (6) The toggles.  With shell-highlight? off the whole line draws plain (the
;; pre-change behaviour -- so the colour assertions above are non-vacuous); with
;; only shell-highlight-paths? off a missing path is no longer flagged while the
;; head still highlights.
;; ======================================================================

(parameterize ([shell-highlight? #f])
  (let ([scr (screen-of "myecho -x")])
    (test-equal "toggle: shell-highlight? off draws the head plain (pre-change)"
      'default (cell-fg (vterm-cell scr 0 2)))
    (test-equal "toggle: shell-highlight? off draws the flag plain"
      'default (cell-fg (vterm-cell scr 0 9)))))

(parameterize ([shell-highlight-paths? #f])
  (let ([scr (screen-of "myecho /no/such/zz")])
    (test-equal "toggle: shell-highlight-paths? off leaves a missing path plain"
      'default (cell-fg (vterm-cell scr 0 9)))
    (test-assert "toggle: shell-highlight-paths? off leaves a missing path un-underlined"
      (not (cell-underline? (vterm-cell scr 0 9))))
    (test-equal "toggle: the head still highlights when only paths are off"
      2 (cell-fg (vterm-cell scr 0 2)))))

;; ======================================================================
;; (7) The autosuggestion ghost render path is unchanged: it still draws in dim
;; grey (256-index 240) at the cursor, so highlighting did not disturb it.
;; ======================================================================

(let* ([gb (buffer-from "myec")]
       [scr (render->screen 80
              (lambda (p) (render-line/suggestion p "> " gb 0 "ho hi" 80)))])
  (test-equal "ghost: the suggestion glyph is drawn past the buffer"
    #\h (cell-glyph (vterm-cell scr 0 6)))
  (test-equal "ghost: the suggestion is dim grey (index 240)"
    240 (cell-fg (vterm-cell scr 0 6))))

(test-end)
