;;; test/test-sexp-textobj.ss -- Coverage for the lexer-aware s-expression text
;;; object: the operator forms dis/das (and the atom is==as identity), the
;;; string and char-literal cases that prove the object never mistakes a paren
;;; inside a string or a #\( literal for structure, and the visual vis/vas
;;; selection. Everything is driven through vi-process-key with bounded string
;;; ports, so the suite needs no terminal and can never block.
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
;; bounded input port. Used for keys that read no follow-up (d, v).
(define (press! es gb kr ch)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string "") (open-output-string) gb kr))

;; Drive an object-taking key: press CH (i or a) with OBJ queued on the bounded
;; input port as the follow-up object key the dispatch reads.
(define (press-obj! es gb kr ch obj)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string (string obj)) (open-output-string) gb kr))

(test-begin "sexp-textobj")

;; --- Operator forms: assert the buffer after the edit -----------------------

;; dis on a list deletes the inner contents, leaving the empty delimiters.
;; "(foo bar)" with the cursor on the opening paren (index 0): d then i s.
(let ()
  (define gb (buffer-with-cursor "(foo bar)" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-obj! es gb kr #\i #\s)
  (test-equal "dis deletes the inner list, leaving the empty delimiters"
    "()" (gap-buffer->string gb)))

;; das on a list deletes the whole form, delimiters included.
(let ()
  (define gb (buffer-with-cursor "(foo bar)" 0))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-obj! es gb kr #\a #\s)
  (test-equal "das deletes the whole list"
    "" (gap-buffer->string gb)))

;; On an atom the object has no delimiters, so is and as are identical: both
;; delete just the atom. "(foo bar)" with the cursor on foo (index 1).
(let ()
  (define gb (buffer-with-cursor "(foo bar)" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-obj! es gb kr #\i #\s)
  (test-equal "dis on an atom deletes just the atom"
    "( bar)" (gap-buffer->string gb)))

(let ()
  (define gb (buffer-with-cursor "(foo bar)" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-obj! es gb kr #\a #\s)
  (test-equal "das on an atom also deletes just the atom (is == as)"
    "( bar)" (gap-buffer->string gb)))

;; --- Lexer-awareness: a paren inside a string / char literal is text --------

;; A ) inside a string is not the closer. "(a \"b)c\" d)" with the cursor on the
;; inter-atom space (index 2): das bounds the whole outer list, not the string.
;; A naive paren scan would stop at the string's ) and mis-delete.
(let ()
  (define gb (buffer-with-cursor "(a \"b)c\" d)" 2))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-obj! es gb kr #\a #\s)
  (test-equal "das spans the whole list; the ) inside the string is text"
    "" (gap-buffer->string gb)))

;; A #\( character literal is not an opener. "(#\\( )" with the cursor on the
;; trailing space (index 4): das bounds the whole list.
(let ()
  (define gb (buffer-with-cursor "(#\\( )" 4))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\d)
  (press-obj! es gb kr #\a #\s)
  (test-equal "das spans the whole list; the #\\( char literal is text"
    "" (gap-buffer->string gb)))

;; --- Visual forms: assert the resolved selection range ----------------------

;; vis on an atom selects the atom. "(foo bar)" with the cursor on foo (index
;; 1): v then i s yields the (1 . 4) span.
(let ()
  (define gb (buffer-with-cursor "(foo bar)" 1))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\v)
  (press-obj! es gb kr #\i #\s)
  (test-equal "vis on an atom selects the atom span"
    '(1 . 4) (vi-visual-range gb)))

;; vis / vas on a nested list. "(a (b c))" with the cursor on the inner ( (index
;; 3): vis selects the contents (4 . 7); starting fresh, vas selects the whole
;; inner form (3 . 8).
(let ()
  (define gb (buffer-with-cursor "(a (b c))" 3))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\v)
  (press-obj! es gb kr #\i #\s)
  (test-equal "vis on a list selects the inner contents"
    '(4 . 7) (vi-visual-range gb)))

(let ()
  (define gb (buffer-with-cursor "(a (b c))" 3))
  (define kr (make-kill-ring))
  (define es (state-for gb kr))
  (vi-reset-session!)
  (press! es gb kr #\v)
  (press-obj! es gb kr #\a #\s)
  (test-equal "vas on a list selects the whole form"
    '(3 . 8) (vi-visual-range gb)))

(test-end)
