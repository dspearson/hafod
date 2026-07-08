;;; test/test-registers.ss -- Coverage for the vim register set: a named yank or
;;; delete populates its register (locking the register-reset ordering fix, so
;;; "ayy no longer loses its target to the unnamed register), an upper-case
;;; A..Z register appends to its lower-case sibling, the "0 register holds the
;;; last yank and survives a later delete, and p / P paste the selected register
;;; then reset back to the unnamed default -- falling back to the kill-ring when
;;; no register is named, so the emacs kills and the dd-then-p behaviour are
;;; unchanged. Everything is driven through vi-process-key and the paste
;;; commands with bounded string ports, so the suite needs no terminal and can
;;; never block.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor gap-buffer)
        (hafod editor kill-ring)
        (hafod editor input-decode)
        (hafod editor vi)
        ;; Importing the editor library installs vi.ss's procedure hooks
        ;; (vi-snapshot!-proc and friends) at load, so vi-process-key runs
        ;; without a null-hook error, and gives us the paste commands.
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
;; bounded input port. Each register/operator keystroke is a separate call, so
;; the state machine advances one event at a time exactly as the main loop
;; drives it (" then the register letter then y y).
(define (press! es gb kr ch)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string "") (open-output-string) gb kr))

;; Drive one key through vi-process-key exactly as the main loop does, then --
;; only when vi does NOT claim it (returns #f) -- fall through to the keymap paste,
;; mirroring the real p / P dispatch. This is the path the direct cmd-paste-* calls
;; bypass: it proves vi-process-key's fall-through leaves the pending "reg selection
;; intact for the paste to read, rather than clobbering it back to the unnamed
;; default before the keymap ever runs.
(define (press-paste! es gb kr ch)
  (unless (vi-process-key es (make-key-event 'char ch 0)
                          (open-input-string "") (open-output-string) gb kr)
    (cond
      [(char=? ch #\p) (cmd-paste-after es)]
      [(char=? ch #\P) (cmd-paste-before es)])))

;; Drive " REG OP OP over a fresh buffer holding TEXT: select register REG, then
;; run the doubled operator OP (yy / dd apply to the whole line). Returns the
;; (gb . es) pair so a follow-up (e.g. a paste) can act on the same state.
(define (reg-op! text reg-ch op-ch)
  (let* ([gb (buffer-with-cursor text 0)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (press! es gb kr #\")
    (press! es gb kr reg-ch)
    (press! es gb kr op-ch)
    (press! es gb kr op-ch)
    (cons gb es)))

;; Drive a plain (unnamed) doubled operator OP OP over TEXT. Returns (gb . es).
(define (plain-op! text op-ch)
  (let* ([gb (buffer-with-cursor text 0)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (press! es gb kr op-ch)
    (press! es gb kr op-ch)
    (cons gb es)))

(test-begin "registers")

;; --- 1. Named store + the reset-ordering fix -------------------------------

;; "ayy stores the whole line under register a. Before the fix, vi-apply-operator!
;; reset register-char to the unnamed default before the yank read it, so
;; register a stayed empty; capturing the register before the reset is what makes
;; this pass. The unnamed register and the "0 yank register hold the text too.
(let ()
  (vi-reset-session!)
  (reg-op! "foo" #\a #\y)
  (test-equal "\"ayy stores the line under register a (reset-ordering fix)"
    "foo" (vi-reg-fetch #\a))
  (test-equal "\"ayy mirrors the yank into the unnamed register"
    "foo" (vi-reg-fetch #\"))
  (test-equal "a yank also fills the \"0 last-yank register"
    "foo" (vi-reg-fetch #\0)))

;; A named delete populates its register too (the same capture path via dd).
(let ()
  (vi-reset-session!)
  (reg-op! "quux" #\c #\d)
  (test-equal "\"cdd stores the deleted line under register c"
    "quux" (vi-reg-fetch #\c)))

;; --- 2. Upper-case A..Z append ----------------------------------------------

;; "ayy seeds register a with "foo"; "Ayy then appends "bar" to the lower-case
;; register a, and the unnamed register mirrors the accumulated text.
(let ()
  (vi-reset-session!)
  (reg-op! "foo" #\a #\y)      ;; seed a = "foo"
  (reg-op! "bar" #\A #\y)      ;; append -> a = "foobar"
  (test-equal "\"Ayy appends onto the lower-case register a"
    "foobar" (vi-reg-fetch #\a))
  (test-equal "the unnamed register mirrors the accumulated append"
    "foobar" (vi-reg-fetch #\")))

;; A lower-case target overwrites rather than appends.
(let ()
  (vi-reset-session!)
  (reg-op! "one" #\d #\y)
  (reg-op! "two" #\d #\y)
  (test-equal "a lower-case register target overwrites"
    "two" (vi-reg-fetch #\d)))

;; --- 3. "0 is yank-only (a delete never clobbers it) ------------------------

;; A plain yank sets "0; a subsequent delete lands in the unnamed register but
;; leaves "0 holding the earlier yank -- proving the delete cannot clobber the
;; last-yanked text.
(let ()
  (vi-reset-session!)
  (plain-op! "aaa" #\y)        ;; yank -> "0 and unnamed = "aaa"
  (test-equal "a plain yank sets the \"0 register"
    "aaa" (vi-reg-fetch #\0))
  (plain-op! "bbb" #\d)        ;; delete -> unnamed = "bbb", "0 untouched
  (test-equal "a later delete lands in the unnamed register"
    "bbb" (vi-reg-fetch #\"))
  (test-equal "the delete leaves the \"0 yank register untouched"
    "aaa" (vi-reg-fetch #\0)))

;; --- 4. Named paste + the selection reset -----------------------------------

;; "ayy yanks "hello" into register a; selecting a again and pasting after the
;; cursor into a fresh (empty) buffer inserts register a's text. The paste then
;; resets the selection, so a second take returns #f (the unnamed signal).
(let ()
  (vi-reset-session!)
  (reg-op! "hello" #\a #\y)
  (let* ([gb (buffer-with-cursor "" 0)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (press! es gb kr #\")
    (press! es gb kr #\a)
    (cmd-paste-after es)
    (test-equal "\"ap pastes the named register a into the buffer"
      "hello" (gap-buffer->string gb))
    (test-assert "the paste resets the register selection to the unnamed default"
      (not (vi-take-paste-register!)))))

;; "bP pastes register b before the cursor (no cursor advance).
(let ()
  (vi-reset-session!)
  (reg-op! "world" #\b #\y)
  (let* ([gb (buffer-with-cursor "" 0)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (press! es gb kr #\")
    (press! es gb kr #\b)
    (cmd-paste-before es)
    (test-equal "\"bP pastes the named register b before the cursor"
      "world" (gap-buffer->string gb))))

;; --- 4b. Named paste through the REAL p / P keypress ------------------------

;; Regression for the fall-through reset bug: in the running editor p / P go
;; through vi-process-key FIRST (as the main loop drives them), and only on its #f
;; fall-through does the keymap paste run. vi-process-key's else arm must NOT reset
;; the pending "a selection, or vi-take-paste-register! reads the reset unnamed
;; default and the paste wrongly falls back to the kill-ring. The kill-ring is
;; pre-loaded with a distinct sentinel so a wrong fallback is unmistakable: pre-fix
;; the buffer would read "KILL", post-fix it reads the register text.
(let ()
  (vi-reset-session!)
  (reg-op! "hello" #\a #\y)
  (let* ([gb (buffer-with-cursor "" 0)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (kill-ring-push! kr "KILL")
    (press! es gb kr #\")
    (press! es gb kr #\a)
    (press-paste! es gb kr #\p)
    (test-equal "\"ap via the real p keypress pastes register a, not the kill-ring"
      "hello" (gap-buffer->string gb))))

;; The same through P (paste before): the fall-through must keep register b live.
(let ()
  (vi-reset-session!)
  (reg-op! "world" #\b #\y)
  (let* ([gb (buffer-with-cursor "" 0)]
         [kr (make-kill-ring)]
         [es (state-for gb kr)])
    (kill-ring-push! kr "KILL")
    (press! es gb kr #\")
    (press! es gb kr #\b)
    (press-paste! es gb kr #\P)
    (test-equal "\"bP via the real P keypress pastes register b, not the kill-ring"
      "world" (gap-buffer->string gb))))

;; --- 5. Unnamed kill-ring fallback (emacs / dd-then-p unchanged) ------------

;; With no register named, dd pushes the deleted text onto the kill-ring and
;; empties the line; a following p falls back to the kill-ring (vi-take-paste-
;; register! returns #f, so cmd-paste-after runs cmd-yank) and pastes it back.
(let ()
  (vi-reset-session!)
  (let ([r (plain-op! "xyz" #\d)])
    (let ([gb (car r)] [es (cdr r)])
      (test-equal "the unnamed delete empties the line"
        "" (gap-buffer->string gb))
      (cmd-paste-after es)
      (test-equal "an unnamed p pastes back via the kill-ring (fallback unchanged)"
        "xyz" (gap-buffer->string gb)))))

(test-end)
