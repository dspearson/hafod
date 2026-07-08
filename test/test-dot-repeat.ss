;;; test/test-dot-repeat.ss -- End-to-end coverage for the . (repeat) engine: the
;;; main-loop change-recording boundary, the synthetic-port replay driver, count
;;; handling and the exclusion set. Each scenario is driven through
;;; editor-drive-keys!, the white-box harness that runs the SAME recording
;;; boundary, initiating-event capture and dispatch the interactive loop runs,
;;; then asserts the resulting buffer (what the line would submit) and, where it
;;; matters, the recorded change bytes.
;;;
;;; A note on the harness: the byte decoder disambiguates a lone Escape from an
;;; Alt sequence with char-ready?, which on a bounded string port is always true
;;; mid-stream -- so a pure read-expression script cannot enter normal mode with
;;; an Escape followed by more keys (it would decode as Meta). editor-drive-keys!
;;; sidesteps this by feeding each keystroke group as its own bounded port (an
;;; isolated Escape then sits at end-of-port and decodes to 'escape, exactly as a
;;; real terminal delivers it), and by accepting a pre-built key-event for a key
;;; the decoder cannot synthesise from bytes (C-_, which arrives as a raw control
;;; byte the decoder maps to a plain char). Every port is bounded, so no read can
;;; block and the suite cannot hang.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor editor)
        (hafod editor gap-buffer)
        (hafod editor kill-ring)
        (hafod editor input-decode))

;; The single Escape byte (isolated in its own chunk it decodes to 'escape).
(define ESC (string (integer->char #x1b)))
;; The s-expression transpose key is M-C-t (Alt+Ctrl-t): Escape then Ctrl-t.
(define TRANSPOSE (string (integer->char #x1b) (integer->char #x14)))
;; The redo key is M-/ (Alt+slash): Escape then '/'; isolated it decodes to Meta.
(define REDO (string-append ESC "/"))

;; Build a normal-mode editor-state around TEXT with the cursor at INDEX, drive
;; the keystroke script INPUTS through the real record/dispatch loop, and return
;; the resulting buffer string.
(define (drive text index inputs)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-insert-string! gb text)
    (gap-buffer-move-cursor! gb (- index (string-length text)))
    (let ([es (make-editor-state gb (make-kill-ring) "> "
                                 (open-output-string) 0 #f #f 'normal #f)])
      (editor-drive-keys! es inputs)
      (gap-buffer->string gb))))

;; Count occurrences of CH in S (for the paredit balance assertion).
(define (char-count s ch)
  (let loop ([i 0] [n 0])
    (cond
      [(= i (string-length s)) n]
      [(char=? (string-ref s i) ch) (loop (+ i 1) (+ n 1))]
      [else (loop (+ i 1) n)])))

(test-begin "dot-repeat")

;; ---- 6. Safe no-op FIRST: with no change ever recorded, . must do nothing ----
;; This runs before any scenario records a change: the committed last change is
;; module-global and persists across prompts, so the "nothing recorded" case is
;; only genuine at the very start.
(test-equal "a bare . with nothing recorded leaves the buffer unchanged"
  "hello"
  (drive "hello" 0 '(".")))
(test-assert "no change is recorded by a lone ."
  (not (editor-dot-last-change)))

;; ---- 1. Operator edit + count ----
;; dw deletes the first word; . deletes the next; the recorded change is the
;; keystrokes "dw", not a descriptor.
(test-equal "the recorded change is the keystroke bytes of the operator edit"
  "dw"
  (begin (drive "a b c" 0 '("dw")) (editor-dot-last-change)))

(test-equal ". repeats an operator edit (dw then . removes two words)"
  "c"
  (drive "a b c" 0 '("dw" ".")))

(test-equal "a count on . loops the operator edit (dw then 3.)"
  "e"
  (drive "a b c d e" 0 '("dw" "3.")))

(test-equal "a count on . loops the operator edit (dw then 2.)"
  "four five"
  (drive "one two three four five" 0 '("dw" "2.")))

;; ---- 2. Insert session + paredit auto-pairing ----
;; Entering insert with a, typing, and Escaping is one change; . replays the
;; keystrokes, so paredit auto-pairing runs again -- balanced, never a
;; double-inserted closer.
(test-equal ". repeats an insert session (a X <Esc> then . appends X again)"
  "fooXX"
  (drive "foo" 2 (list "aX" ESC ".")))

(let ([repeated (drive "foo" 2 (list "a(" ESC "."))])
  (test-equal ". reproduces paredit auto-pairing exactly (balanced, not double-closed)"
    "foo (())"
    repeated)
  (test-equal "the replayed insert keeps opening and closing parens balanced"
    (char-count repeated #\()
    (char-count repeated #\))))

(test-equal "a count on . repeats an insert three times (a Q <Esc> then 3.)"
  "zQQQQ"
  (drive "z" 0 (list "aQ" ESC "3.")))

;; ---- 3. Multi-key surround (operands read inline via the synthetic port) ----
;; ysiw( surrounds the inner word; moving to the next word and pressing . surrounds
;; it too, proving the inline-read operand (iw) and delimiter (() replay from the
;; synthetic port.
(test-equal ". repeats a multi-key surround whose operands are inline-read"
  "( aa ) ( bb )"
  (drive "aa bb" 0 '("ysiw(" "$" ".")))

;; ---- 4. S-expression transpose ----
;; Transpose swaps the form at point with its next sibling; . applies a second
;; transpose at the new point.
(test-equal ". repeats an s-expression transpose"
  "b a d c"
  (drive "a b c d" 0 (list TRANSPOSE ".")))

;; ---- 5. Exclusion set + no runaway recursion ----
;; Pressing . twice repeats the ORIGINAL change each time (never "repeat the
;; repeat"): dw then . then . removes three words in total.
(test-equal ". repeated twice repeats the original change each time"
  "d"
  (drive "a b c d" 0 '("dw" "." ".")))

;; A count on . followed by a bare . terminates and still repeats the original dw
;; (the N. change is never itself recorded, so . never replays a .).
(test-equal "N. then . terminates and repeats the original change"
  "f g"
  (drive "a b c d e f g" 0 '("dw" "3." ".")))

;; vi undo (u) does not overwrite the last change: after dw then u, . still repeats
;; the ORIGINAL dw.
(test-equal "vi undo (u) does not overwrite the last change"
  "b c"
  (drive "a b c" 0 '("dw" "u" ".")))
(test-equal "the last change is still dw after an intervening u"
  "dw"
  (begin (drive "a b c" 0 '("dw" "u")) (editor-dot-last-change)))

;; The emacs undo C-_ (a raw ctrl event the byte decoder cannot synthesise) does
;; not overwrite the last change either.
(test-equal "emacs undo (C-_) does not overwrite the last change"
  "b c"
  (drive "a b c" 0 (list "dw" (make-key-event 'ctrl #\_ 0) ".")))

;; The emacs/vim redo M-/ does not overwrite the last change: after dw then u then
;; M-/ (which redoes the delete, changing the buffer), . still repeats the dw.
(test-equal "redo (M-/) does not overwrite the last change"
  "c"
  (drive "a b c" 0 (list "dw" "u" REDO ".")))

;; Paste (p) is excluded too: after dw then p, . still repeats the dw.
(test-equal "the last change is still dw after an intervening p (paste)"
  "dw"
  (begin (drive "a b c" 0 '("dw" "p")) (editor-dot-last-change)))

;; ---- 5b. A leading count must not defeat the exclusion set ----
;; A count makes the change's initiating key a digit, so classifying the change by
;; its first key alone would let a counted undo / redo / paste be committed as the
;; last change; a later . would then replay the undo/redo/paste. The change is
;; instead classified by what it DID, so the counted forms stay excluded and the
;; ORIGINAL dw remains the last change.

;; Counted undo (2u): after dw then 2u the last change is still dw.
(test-equal "the last change is still dw after a counted undo (2u)"
  "dw"
  (begin (drive "a b c d e" 0 '("dw" "2u")) (editor-dot-last-change)))
;; ... and . then still replays the ORIGINAL dw rather than the counted undo. The
;; two dw edits set up two self-contained undo levels so 2u restores exactly to the
;; pristine line (the undo stack is module-global and accumulates across drives, so
;; a lone dw + 2u would over-undo into a previous scenario's snapshot); the last
;; recorded change is dw throughout, and the trailing . deletes the first word.
(test-equal "a counted undo (2u) does not defeat the exclusion; . still repeats dw"
  "b c d e"
  (drive "a b c d e" 0 '("dw" "dw" "2u" ".")))

;; Counted paste (3"zp): seed register z, delete a word (the recorded change), then
;; do a COUNTED named paste. Its initiating key is the digit 3, so classifying the
;; change by its first key would let it slip past the exclusion set and overwrite
;; the last change; the paste taint keeps dw as the last change. A named register is
;; used deliberately -- the drive harness hands cmd-yank a private kill-ring, so an
;; unnamed paste would be a no-op here, whereas the register path drives the real
;; paste mutation and so genuinely mutates the buffer pre-fix.
(test-equal "the last change is still dw after a counted named paste (3\"zp)"
  "dw"
  (begin (drive "a b c" 0 '("\"zyy" "dw" "3\"zp")) (editor-dot-last-change)))

;; Counted paste-before (2"zP) is excluded on the same footing.
(test-equal "the last change is still dw after a counted named paste-before (2\"zP)"
  "dw"
  (begin (drive "a b c" 0 '("\"zyy" "dw" "2\"zP")) (editor-dot-last-change)))

(test-end)
