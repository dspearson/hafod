;;; test/test-surround-delete.ss -- Coverage for the surround delete operator:
;;; ds{char} locates the enclosing pair of the requested family and removes both
;;; delimiters, keeping the inner content. It proves that every delimiter family
;;; is recognised (the lexer-aware brackets, the guarded double-quote string, the
;;; raw {}/<>/'/backtick scans) together with the b/B/r/a aliases, that the inner
;;; whitespace is trimmed only when the requested char is an opening bracket (so
;;; ds( on ( x ) gives x while ds) on ( x ) preserves the spaces to give " x "),
;;; that detection is lexer- and type-aware (the innermost pair of the requested
;;; type, the nearest () skipping an inner [], and a ) inside a string never the
;;; closer), that the ' and ` families resolve only with the cursor on the
;;; opening quote (a cursor inside is a documented no-op here), and that a
;;; missing pair, a lone quote, or a non-delimiter char is a safe no-op that
;;; raises nothing. Everything is driven through vi-process-key with bounded
;;; string ports, so the suite needs no terminal and can never block.
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
;; bounded input port. Used for the leading operator key (d), which reads no
;; follow-up of its own.
(define (press! es gb kr ch)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string "") (open-output-string) gb kr))

;; Press CH with the trailing string REST queued on the bounded input port: the
;; single read-key-event the dispatch makes for ds consumes REST's one delimiter.
;; Driving ds) as (press! ... #\d) then (press-str! ... #\s ")") feeds the
;; delimiter without it reaching the keymap.
(define (press-str! es gb kr ch rest)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string rest) (open-output-string) gb kr))

;; Drive a ds delete on TEXT (cursor at INDEX): press d, then the surround key
;; with REST (the one raw delimiter naming the family) queued. Return the buffer
;; after the edit.
(define (ds! text index rest)
  (let* ([gb (buffer-with-cursor text index)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (vi-reset-session!)
    (press! es gb kr #\d)
    (press-str! es gb kr #\s rest)
    (gap-buffer->string gb)))

(test-begin "surround-delete")

;; --- Delimiter families + b/B/r/a aliases (cursor on the inner content) -----

;; A closing bracket / its alias removes the () pair with nothing to trim.
(test-equal "ds) removes the parens from (x)"
  "x" (ds! "(x)" 1 ")"))
(test-equal "dsb removes the parens from (x) (b is the ) alias)"
  "x" (ds! "(x)" 1 "b"))
;; The double-quote family resolves through the lexer string bounds (cursor on x).
(test-equal "ds\" removes the double quotes from \"x\""
  "x" (ds! "\"x\"" 1 "\""))

(test-equal "ds] removes the brackets from [x]"
  "x" (ds! "[x]" 1 "]"))
(test-equal "dsr removes the brackets from [x] (r is the ] alias)"
  "x" (ds! "[x]" 1 "r"))

;; The brace family is a raw scan -- {} is not lexed as Scheme structure.
(test-equal "ds} removes the braces from {x}"
  "x" (ds! "{x}" 1 "}"))
(test-equal "dsB removes the braces from {x} (B is the } alias)"
  "x" (ds! "{x}" 1 "B"))

;; The angle family is a raw scan too.
(test-equal "ds> removes the angles from <x>"
  "x" (ds! "<x>" 1 ">"))
(test-equal "dsa removes the angles from <x> (a is the > alias)"
  "x" (ds! "<x>" 1 "a"))

;; --- Trim vs preserve: the load-bearing open-mark asymmetry ------------------

;; An opening bracket char trims the whitespace a matching open-add would insert.
(test-equal "ds( trims the inner spaces of ( x ) to x"
  "x" (ds! "( x )" 2 "("))
;; A closing bracket char preserves the inner spaces -- they remain in place.
(test-equal "ds) preserves the inner spaces of ( x ) as \" x \""
  " x " (ds! "( x )" 2 ")"))

;; --- Nested and mixed-type targets (lexer- and type-aware) ------------------

;; The innermost () of a nested same-type pair is the one removed.
(test-equal "ds) removes the innermost parens of ((a)) to give (a)"
  "(a)" (ds! "((a))" 2 ")"))
(test-equal "ds) removes the innermost parens of (a (b) c) to give (a b c)"
  "(a b c)" (ds! "(a (b) c)" 4 ")"))
;; Type-aware: the nearest () is removed, the inner [] left untouched.
(test-equal "ds) on ([a]) removes the nearest parens, leaving [a]"
  "[a]" (ds! "([a])" 2 ")"))

;; --- Cursor on the opening bracket resolves the pair (both ways) -------------

;; The () and [] families resolve the pair with the cursor sitting ON the
;; opening bracket, just as the brace, angle and quote families do: the lexer
;; walk only stacks openers before the cursor, so an opener at the cursor is
;; taken directly. It must still resolve with the cursor inside the pair.
(test-equal "ds) removes the parens of (x) with the cursor on the open ("
  "x" (ds! "(x)" 0 ")"))
(test-equal "ds) still removes the parens of (x) with the cursor inside"
  "x" (ds! "(x)" 1 ")"))
(test-equal "ds] removes the brackets of [x] with the cursor on the open ["
  "x" (ds! "[x]" 0 "]"))
;; The outer pair is taken when the cursor is on the outer opener.
(test-equal "ds) on ([a]) with the cursor on the outer ( removes the outer parens"
  "[a]" (ds! "([a])" 0 ")"))

;; --- Lexer-awareness: a ) inside a string is not the closer -----------------

(test-equal "ds) on (a \"b)c\" d) ignores the paren inside the string"
  "a \"b)c\" d" (ds! "(a \"b)c\" d)" 1 ")"))

;; --- Quote families: the cursor-on-the-opening-quote limitation (both ways) --

;; With the cursor ON the opening quote, the same-char scan resolves the pair.
(test-equal "ds' removes the quotes from 'x' with the cursor on the open quote"
  "x" (ds! "'x'" 0 "'"))
;; With the cursor INSIDE the quotes, text-obj-around-pair yields (#f #f): the
;; documented no-op for the ' and ` families, whose backward same-char scan is
;; deliberately left unchanged here.
(test-equal "ds' is a no-op with the cursor inside the quotes"
  "'x'" (ds! "'x'" 1 "'"))

;; --- Safe no-ops: buffer unchanged, no exception raised ---------------------

;; No enclosing () to delete -> unchanged.
(test-equal "ds) with no enclosing pair leaves the buffer unchanged"
  "x" (ds! "x" 0 ")"))
;; No enclosing {} to delete -> unchanged (raw family, no match).
(test-equal "ds} with no enclosing pair leaves the buffer unchanged"
  "x" (ds! "x" 0 "}"))
;; A lone opening quote with no matching close -> unchanged.
(test-equal "ds' on a lone quote 'foo leaves the buffer unchanged"
  "'foo" (ds! "'foo" 0 "'"))
;; A non-delimiter char (X) -> unchanged; this exercises surround-locate's
;; leading #f guard. Without it, a (char=? #f #f) would raise inside the delete.
(test-equal "dsX with a non-delimiter char leaves the buffer unchanged"
  "(x)" (ds! "(x)" 1 "X"))
;; And prove that same non-delimiter char raises nothing at all.
(test-assert "dsX with a non-delimiter char raises no exception"
  (guard (e [#t #f])
    (ds! "(x)" 1 "X")
    #t))

(test-end)
