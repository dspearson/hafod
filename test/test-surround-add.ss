;;; test/test-surround-add.ss -- Coverage for the surround add operator: the
;;; ys{operand}{char} operator over motions, i/a and lexer-aware sexp text
;;; objects, and the whole-line yss form, plus the visual S{char} surround. It
;;; proves the delimiter/spacing table (an opening bracket adds one inner space
;;; per side, a closing bracket and the b/B/r/a aliases add none, the quotes are
;;; symmetric), locks the < open-angle divergence, and checks that an
;;; unrecognised delimiter or an unresolved operand leaves the buffer untouched
;;; and the state reset. Everything is driven through vi-process-key with
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
;; bounded input port. Used for keys that read no follow-up (y, v, $).
(define (press! es gb kr ch)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string "") (open-output-string) gb kr))

;; Press CH with the whole trailing string REST queued on the bounded input
;; port: the successive read-key-event calls the dispatch makes consume REST in
;; order. Driving ysiw( as (press! ... #\y) then (press-str! ... #\s "iw(")
;; feeds the operand and delimiter without any of them reaching the keymap.
(define (press-str! es gb kr ch rest)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string rest) (open-output-string) gb kr))

;; Drive a ys add on TEXT (cursor at INDEX): press y, then the surround key with
;; REST (the operand and delimiter) queued. Return the buffer after the edit.
(define (ys! text index rest)
  (let* ([gb (buffer-with-cursor text index)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (vi-reset-session!)
    (press! es gb kr #\y)
    (press-str! es gb kr #\s rest)
    (gap-buffer->string gb)))

;; Drive a visual S surround: enter characterwise visual on TEXT (cursor at
;; INDEX), extend with MOTION-CH, then press S with the delimiter REST queued.
;; Return the buffer after the edit.
(define (visual-S! text index motion-ch rest)
  (let* ([gb (buffer-with-cursor text index)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (vi-reset-session!)
    (press! es gb kr #\v)
    (press! es gb kr motion-ch)
    (press-str! es gb kr #\S rest)
    (gap-buffer->string gb)))

(test-begin "surround-add")

;; --- Spacing table + b/B/r/a aliases (operand iw on "word", cursor index 1) --

;; Parentheses: the opening ( adds an inner space per side; the closing ) and
;; its alias b add none.
(test-equal "ysiw( wraps the word with inner-spaced parens"
  "( word )" (ys! "word" 1 "iw("))
(test-equal "ysiw) wraps the word with tight parens"
  "(word)" (ys! "word" 1 "iw)"))
(test-equal "ysiwb (alias for the close paren) wraps with tight parens"
  "(word)" (ys! "word" 1 "iwb"))

;; Brackets: [ adds spaces, ] and its alias r add none.
(test-equal "ysiw[ wraps the word with inner-spaced brackets"
  "[ word ]" (ys! "word" 1 "iw["))
(test-equal "ysiw] wraps the word with tight brackets"
  "[word]" (ys! "word" 1 "iw]"))
(test-equal "ysiwr (alias for the close bracket) wraps with tight brackets"
  "[word]" (ys! "word" 1 "iwr"))

;; Braces: { adds spaces, } and its alias B add none.
(test-equal "ysiw{ wraps the word with inner-spaced braces"
  "{ word }" (ys! "word" 1 "iw{"))
(test-equal "ysiw} wraps the word with tight braces"
  "{word}" (ys! "word" 1 "iw}"))
(test-equal "ysiwB (alias for the close brace) wraps with tight braces"
  "{word}" (ys! "word" 1 "iwB"))

;; Angles: this editor repurposes < as the space-adding open-angle (vim opens a
;; tag prompt); > and its alias a add none. Lock the divergence.
(test-equal "ysiw< wraps the word with inner-spaced angles (open-angle divergence)"
  "< word >" (ys! "word" 1 "iw<"))
(test-equal "ysiw> wraps the word with tight angles"
  "<word>" (ys! "word" 1 "iw>"))
(test-equal "ysiwa (alias for the close angle) wraps with tight angles"
  "<word>" (ys! "word" 1 "iwa"))

;; Quotes: symmetric, no inner space.
(test-equal "ysiw\" wraps the word in double quotes"
  "\"word\"" (ys! "word" 1 "iw\""))
(test-equal "ysiw' wraps the word in single quotes"
  "'word'" (ys! "word" 1 "iw'"))
(test-equal "ysiw` wraps the word in backticks"
  "`word`" (ys! "word" 1 "iw`"))

;; --- Operands: motion, lexer-aware sexp object, whole line ------------------

;; Motion: ys$ over "word" from column 0 wraps to end of line; the $ motion is
;; inclusive of the last character (the motion arm's end fixup).
(test-equal "ys$) wraps the whole line via the inclusive $ motion"
  "(word)" (ys! "word" 0 "$)"))

;; Around-sexp object: with the cursor on the inner ) of (a (b) c) (index 5),
;; ysas( wraps the whole inner list, honouring the lexer-aware sexp object, and
;; the opening ( adds the inner spaces.
(test-equal "ysas( wraps the around-sexp inner list"
  "(a ( (b) ) c)" (ys! "(a (b) c)" 5 "as("))

;; Whole line: yss wraps from the first non-blank to end of line, so the leading
;; whitespace is preserved outside the pair.
(test-equal "yss( wraps the whole line from first-non-blank, leading ws preserved"
  "  ( hi there )" (ys! "  hi there" 2 "s("))

;; --- Visual S{char} ---------------------------------------------------------

;; Select "word" characterwise (v then $ from column 0), then S with a delimiter.
(test-equal "visual S) surrounds the selection with tight parens"
  "(word)" (visual-S! "word" 0 #\$ ")"))
(test-equal "visual S( surrounds the selection with inner-spaced parens"
  "( word )" (visual-S! "word" 0 #\$ "("))

;; An aborted visual S -- an unrecognised delimiter -- leaves the buffer
;; untouched but must still exit visual mode, so the selection state agrees with
;; the resolved-delimiter path rather than stranding an active selection.
(let* ([gb (buffer-with-cursor "word" 0)]
       [kr (make-kill-ring)]
       [es (state-for gb kr)])
  (vi-reset-session!)
  (press! es gb kr #\v)
  (press! es gb kr #\$)
  (press-str! es gb kr #\S "X")
  (test-equal "an aborted visual S (unknown delimiter) leaves the buffer unchanged"
    "word" (gap-buffer->string gb))
  (test-assert "an aborted visual S clears the visual selection"
    (not (vi-visual-mode))))

;; --- Safe no-ops: unchanged buffer AND reset state --------------------------

;; An unrecognised delimiter (X) after a resolved operand leaves the buffer
;; untouched and no snapshot pushed; the dispatch still resets to normal.
(let* ([gb (buffer-with-cursor "word" 1)]
       [kr (make-kill-ring)]
       [es (state-for gb kr)])
  (vi-reset-session!)
  (press! es gb kr #\y)
  (press-str! es gb kr #\s "iwX")
  (test-equal "ysiwX with an invalid delimiter leaves the buffer unchanged"
    "word" (gap-buffer->string gb))
  (test-assert "an invalid-delimiter surround still resets to normal"
    (eq? 'normal (vi-pending-state))))

;; An operand that resolves to no span (i( with no enclosing parens) is a no-op.
(let* ([gb (buffer-with-cursor "word" 0)]
       [kr (make-kill-ring)]
       [es (state-for gb kr)])
  (vi-reset-session!)
  (press! es gb kr #\y)
  (press-str! es gb kr #\s "i(")
  (test-equal "ysi( with no enclosing pair leaves the buffer unchanged"
    "word" (gap-buffer->string gb))
  (test-assert "an unresolved-operand surround still resets to normal"
    (eq? 'normal (vi-pending-state))))

;; --- Unresolved operand must still drain its delimiter off the port ---------

;; ys reads the operand and the trailing delimiter straight from the port, so
;; neither can reach the keymap. That has to hold even when the operand resolves
;; to no span -- a garbage operand, or an iw with no word under the cursor at
;; the end of the buffer. Were the delimiter left unread it would surface as a
;; fresh keystroke on the next pass of the loop and self-insert or auto-pair,
;; mutating the buffer behind the user's back. Drive the operator with the
;; operand, its delimiter and a trailing sentinel on ONE port; afterwards the
;; next key on the port must be the sentinel (the delimiter having been drained),
;; not the delimiter itself, and the buffer must be untouched.

;; Return (buffer . next-port-char) after a ys over REST (the operand, its
;; delimiter and a sentinel) driven on a single shared port; next-port-char is
;; the character still queued after the edit, or #f at end of port.
(define (ys-delim-tail text index rest)
  (let* ([gb (buffer-with-cursor text index)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)]
         [port (open-input-string rest)])
    (vi-reset-session!)
    (press! es gb kr #\y)
    ;; feed the surround key sharing PORT so the tail can be inspected
    (vi-process-key es (make-key-event 'char #\s 0)
                    port (open-output-string) gb kr)
    (let ([e (read-key-event port)])
      (cons (gap-buffer->string gb)
            (and (not (eof-object? e))
                 (eq? (key-event-type e) 'char)
                 (key-event-value e))))))

;; A garbage operand (p is neither a motion nor an i/a object) over "word": the
;; ( delimiter is drained, so the sentinel Z is the next key and "word" stands.
(let ([r (ys-delim-tail "word" 1 "p(Z")])
  (test-equal "ysp( with a garbage operand leaves the buffer unchanged"
    "word" (car r))
  (test-equal "ysp( drains the delimiter (sentinel, not the ( , is next)"
    #\Z (cdr r)))

;; An iw operand with no word under the cursor (empty buffer, pos >= len yields
;; a #f span): the ( delimiter is still drained and the sentinel Z remains.
(let ([r (ys-delim-tail "" 0 "iw(Z")])
  (test-equal "ysiw( at end-of-buffer leaves the buffer unchanged"
    "" (car r))
  (test-equal "ysiw( at end-of-buffer drains the delimiter to the sentinel"
    #\Z (cdr r)))

(test-end)
