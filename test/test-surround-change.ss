;;; test/test-surround-change.ss -- Coverage for the surround change operator:
;;; cs{old}{new} locates the enclosing pair of the old family and re-wraps it in
;;; the new pair. It proves that the new char's family sets the emitted spacing
;;; (an opening bracket adds an inner space per side, a closer / alias / quote
;;; adds none) while the old char's family sets the trimming (an opening-bracket
;;; old strips the inner leading/trailing whitespace first; a closer / alias /
;;; quote old preserves it), that the b/B/r/a aliases resolve on both sides, that
;;; bracket detection is lexer- and type-aware (the innermost pair of the
;;; requested type, a ) inside a string never the closer), that the quote
;;; families resolve from the opening quote and from inside the quotes, and that a
;;; missing pair, an invalid new delimiter, or a non-delimiter old char is a safe
;;; no-op that raises nothing. Everything is driven through vi-process-key with
;;; bounded string ports, so the suite needs no terminal and can never block.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor gap-buffer)
        (hafod editor kill-ring)
        (hafod editor input-decode)
        (hafod editor vi)
        ;; Importing the editor library installs vi.ss's procedure hooks
        ;; (vi-snapshot!-proc and friends) at load, so vi-process-key runs
        ;; without a null-hook error.
        (hafod editor editor))

;; Build a single-line gap-buffer holding TEXT with the cursor at INDEX.
(define (buffer-with-cursor text index)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert-string! gb text)
    ;; insert leaves the cursor at the end; step it back to the chosen index
    (gap-buffer-move-cursor! gb (- index (string-length text)))
    gb))

;; Construct a normal-mode editor-state around a buffer. The trailing #f is the
;; initial mark (no region active).
(define (state-for gb kr)
  (make-editor-state gb kr "> " (open-output-string) 0 #f #f 'normal #f))

;; Feed one character key event through the vi state machine with an empty,
;; bounded input port. Used for the leading operator key (c), which reads no
;; follow-up of its own.
(define (press! es gb kr ch)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string "") (open-output-string) gb kr))

;; Press CH with the whole trailing string REST queued on the bounded input
;; port: the successive read-key-event calls the dispatch makes consume REST in
;; order. Driving cs)] as (press! ... #\c) then (press-str! ... #\s ")]") feeds
;; the old and new delimiters without either reaching the keymap.
(define (press-str! es gb kr ch rest)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string rest) (open-output-string) gb kr))

;; Drive a cs change on TEXT (cursor at INDEX): press c, then the surround key
;; with REST (the two raw delimiters, old then new) queued. Return the buffer
;; after the edit.
(define (cs! text index rest)
  (let* ([gb (buffer-with-cursor text index)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (vi-reset-session!)
    (press! es gb kr #\c)
    (press-str! es gb kr #\s rest)
    (gap-buffer->string gb)))

(test-begin "surround-change")

;; --- Spacing set by the new char + b/B/r/a aliases (cursor on the inner x) ---

;; The new char's family sets the spacing: a closer adds none, an opening bracket
;; adds one inner space per side. cs)] proves the let* read order -- old = ) is
;; read first, new = ] second; a swap would look for a [] that is not there.
(test-equal "cs)] changes (x) to tight brackets"
  "[x]" (cs! "(x)" 1 ")]"))
(test-equal "cs)[ changes (x) to inner-spaced brackets (new open bracket)"
  "[ x ]" (cs! "(x)" 1 ")["))
(test-equal "cs)} changes (x) to tight braces"
  "{x}" (cs! "(x)" 1 ")}"))
(test-equal "cs){ changes (x) to inner-spaced braces (new open brace)"
  "{ x }" (cs! "(x)" 1 "){"))

;; Aliases resolve on both sides: b = ) on the old side, r = ] on the new.
(test-equal "csbr changes (x) to tight brackets (aliases on both sides)"
  "[x]" (cs! "(x)" 1 "br"))
;; Quote to quote, no spaces (cursor on x, index 1 of "x").
(test-equal "cs\"' changes a double-quoted x to single quotes"
  "'x'" (cs! "\"x\"" 1 "\"'"))

;; --- Trim driven by the old char's family (both directions) -----------------

;; A close-mark old preserves the existing inner spaces; the new ] adds none.
(test-equal "cs)] on ( x ) preserves the inner spaces (close-mark old)"
  "[ x ]" (cs! "( x )" 2 ")]"))
;; An open-mark old trims the inner spaces first; the new ] then adds none.
(test-equal "cs(] on ( x ) trims the inner spaces (open-mark old)"
  "[x]" (cs! "( x )" 2 "(]"))
;; Trim then re-add: an open-mark old strips the spaces, an open-mark new re-adds.
(test-equal "cs([ on ( x ) trims then re-adds the inner spaces"
  "[ x ]" (cs! "( x )" 2 "(["))

;; --- Nested and mixed-type targets (lexer- and type-aware) ------------------

;; The innermost () of a nested same-type pair is the one changed.
(test-equal "cs)] on ((a)) changes the innermost parens"
  "([a])" (cs! "((a))" 2 ")]"))
(test-equal "cs)] on (a (b) c) changes the innermost parens"
  "(a [b] c)" (cs! "(a (b) c)" 4 ")]"))
;; Type-aware: the nearest () is found, skipping the inner [].
(test-equal "cs)] on ([a]) finds the nearest parens, skipping the brackets"
  "[[a]]" (cs! "([a])" 2 ")]"))

;; --- Cursor on the opening bracket resolves the pair (both ways) -------------

;; The () and [] families resolve the pair with the cursor ON the opening
;; bracket as the other families do (an opener at the cursor is not on the lexer
;; stack, so it is taken directly). It must still resolve with the cursor inside.
(test-equal "cs)] changes (x) to [x] with the cursor on the open ("
  "[x]" (cs! "(x)" 0 ")]"))
(test-equal "cs)] still changes (x) to [x] with the cursor inside"
  "[x]" (cs! "(x)" 1 ")]"))

;; --- Lexer-awareness: a ) inside a string is not the closer -----------------

(test-equal "cs)] on (a \"b)c\" d) ignores the paren inside the string"
  "[a \"b)c\" d]" (cs! "(a \"b)c\" d)" 1 ")]"))

;; --- Quote families: resolved from the opening quote AND from inside ---------

;; With the cursor ON the opening quote, the enclosing pair resolves.
(test-equal "cs'\" resolves with the cursor on the opening quote"
  "\"x\"" (cs! "'x'" 0 "'\""))
;; And with the cursor INSIDE the quotes: the ' and ` families now resolve the
;; enclosing pair from any inside position, in step with the double-quote family.
(test-equal "cs'\" resolves with the cursor inside the quotes"
  "\"x\"" (cs! "'x'" 1 "'\""))

;; --- Safe no-ops: buffer unchanged, no exception raised ---------------------

;; No enclosing () to change -> unchanged.
(test-equal "cs)] with no enclosing pair leaves the buffer unchanged"
  "x" (cs! "x" 0 ")]"))
;; Invalid NEW delimiter (X) -> unchanged.
(test-equal "cs)X with an invalid new delimiter leaves the buffer unchanged"
  "(x)" (cs! "(x)" 1 ")X"))
;; Non-delimiter OLD char (X) -> unchanged; this exercises surround-locate's
;; leading #f guard. Without it, a (char=? #f #f) would raise inside the change.
(test-equal "csX] with a non-delimiter old char leaves the buffer unchanged"
  "(x)" (cs! "(x)" 1 "X]"))
;; And prove that same non-delimiter old char raises nothing at all.
(test-assert "csX] with a non-delimiter old char raises no exception"
  (guard (e [#t #f])
    (cs! "(x)" 1 "X]")
    #t))

(test-end)
