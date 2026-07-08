;;; test/test-dot-repeat-arm.ss -- Coverage for the . (repeat) arm's delegation
;;; surface: the vi-replay!-proc hook and the vi-mid-command? signal the repeat
;;; engine plugs into. It proves the . arm reads the count typed before the dot
;;; and hands it to the hook (a bare . passes 0, 3. passes 3), that the count is
;;; read before the reset zeroes it (so it never leaks into the next dot), that
;;; an unset (#f) hook makes . a safe no-op leaving the buffer untouched, and
;;; that vi-mid-command? is #t for a pending operator, a register prefix and an
;;; accumulating count but #f from idle normal. Everything is driven through
;;; vi-process-key with bounded string ports, so the suite needs no terminal and
;;; can never block.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor gap-buffer)
        (hafod editor kill-ring)
        (hafod editor input-decode)
        (hafod editor vi)
        ;; Importing the editor library installs vi.ss's procedure hooks
        ;; (vi-snapshot!-proc and friends) at load, so vi-process-key runs
        ;; without a null-hook error. It leaves vi-replay!-proc unset (#f) for
        ;; this suite to drive.
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
;; bounded input port: the . arm and the operator / register / digit prefixes
;; read no follow-up, so the empty port is never consumed and cannot block.
(define (press! es gb kr ch)
  (vi-process-key es (make-key-event 'char ch 0)
                  (open-input-string "") (open-output-string) gb kr))

;; The stub replay hook records each count it is handed, most recent first.
(define captured '())
(define (reset-capture!) (set! captured '()))

(test-begin "dot-repeat-arm")

;; ---- delegation + count: the . arm calls the hook with the typed count ----
(vi-replay!-proc (lambda (es count) (set! captured (cons count captured))))

;; 1. A bare . in idle normal mode delegates once with count 0 and is handled.
(let* ([gb (buffer-with-cursor "hello" 0)]
       [kr (make-kill-ring)]
       [es (state-for gb kr)])
  (vi-reset-session!)
  (reset-capture!)
  (let ([handled (press! es gb kr #\.)])
    (test-equal "a bare . delegates to the hook exactly once" 1 (length captured))
    (test-equal "a bare . passes count 0 (no count typed)" '(0) captured)
    (test-assert "the . arm returns #t (handled)" handled)))

;; 2. 3. reads the count accumulated before the dot and passes 3, then resets.
(let* ([gb (buffer-with-cursor "hello" 0)]
       [kr (make-kill-ring)]
       [es (state-for gb kr)])
  (vi-reset-session!)
  (reset-capture!)
  (press! es gb kr #\3)
  (press! es gb kr #\.)
  (test-equal "3. delegates exactly once" 1 (length captured))
  (test-equal "3. passes the count 3 read before the reset" '(3) captured)
  ;; The arm's reset fired: no operator/pending/count left in flight.
  (test-assert "the arm reset the state (mid-command? is #f after 3.)"
               (not (vi-mid-command?)))
  ;; A following bare . therefore reads count 0 again — the count does not leak.
  (reset-capture!)
  (press! es gb kr #\.)
  (test-equal "a following bare . reads count 0 (no leak)" '(0) captured))

;; ---- safe no-op when no engine is wired (the hook is #f) ----
(vi-replay!-proc #f)
(let* ([gb (buffer-with-cursor "hello" 2)]
       [kr (make-kill-ring)]
       [es (state-for gb kr)])
  (vi-reset-session!)
  (let ([handled (press! es gb kr #\.)])
    (test-equal "a . with the hook unset leaves the buffer untouched"
                "hello" (gap-buffer->string gb))
    (test-assert "a . with the hook unset is still handled (#t)" handled)))

;; ---- vi-mid-command? state transitions ----
;; 3. idle normal → #f; d (operator pending) → #t; reset → #f.
(let* ([gb (buffer-with-cursor "hello" 0)]
       [kr (make-kill-ring)]
       [es (state-for gb kr)])
  (vi-reset-session!)
  (test-assert "idle normal: mid-command? is #f" (not (vi-mid-command?)))
  (press! es gb kr #\d)
  (test-assert "after d (operator pending): mid-command? is #t" (vi-mid-command?))
  (vi-reset-session!)
  (test-assert "after a reset: mid-command? is #f again" (not (vi-mid-command?))))

;; 4. " (register prefix) → pending is not 'normal → #t.
(let* ([gb (buffer-with-cursor "hello" 0)]
       [kr (make-kill-ring)]
       [es (state-for gb kr)])
  (vi-reset-session!)
  (press! es gb kr #\")
  (test-assert "after \" (register pending): mid-command? is #t" (vi-mid-command?))
  (vi-reset-session!))

;; 5. 3 (count accumulating) → count > 0 → #t.
(let* ([gb (buffer-with-cursor "hello" 0)]
       [kr (make-kill-ring)]
       [es (state-for gb kr)])
  (vi-reset-session!)
  (press! es gb kr #\3)
  (test-assert "after 3 (count accumulating): mid-command? is #t" (vi-mid-command?))
  (vi-reset-session!))

;; Leave no global stub set for the other suites.
(vi-replay!-proc #f)

(test-end)
