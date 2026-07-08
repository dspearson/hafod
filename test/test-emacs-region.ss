;;; Emacs mark / region suite -- exercises C-Space set-mark, M-w copy-region and
;;; the region-aware C-w entirely through the black-box editor driver (no PTY).
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor editor)
        (hafod editor gap-buffer)
        (hafod editor input-decode)
        (hafod editor keymap)
        (hafod editor kill-ring)
        (hafod editor history)
        (hafod editor sexp-tracker))

;; Helper: construct a control character (byte 1..26).  Space (C-Space) is byte
;; 0, below this range, so it is spelled directly as (integer->char 0) below.
(define (ctrl ch)
  (integer->char (- (char->integer ch) 96)))

;; Helper: construct a meta sequence (ESC + char), e.g. M-w = ESC w.
(define (meta ch)
  (string (integer->char #x1b) ch))

;; C-Space arrives as the NUL byte.
(define c-space (string (integer->char 0)))

;; C-j (LF, char 10) submits in insert mode.
(define submit (integer->char 10))

;; Helper: drive read-expression over a string of key bytes, return its result.
(define (editor-simulate prompt input)
  (let ([in-port (open-input-string input)]
        [out-port (open-output-string)])
    (read-expression prompt in-port out-port)))

(test-begin "emacs-region")

;; Copy round-trip: mark at start, point at end, M-w copies mark..point without
;; touching the buffer, C-y yanks the copy at the end -> the text is doubled.
(test-equal "M-w copies the region; C-y yanks it back"
  "hellohello"
  (editor-simulate "> "
    (string-append "hello"
                   (string (ctrl #\a))   ; to start
                   c-space               ; set mark at 0
                   (string (ctrl #\e))   ; point to end -> region "hello"
                   (meta #\w)            ; copy region (buffer unchanged)
                   (string (ctrl #\y))   ; yank the copy at end
                   (string submit))))

;; Kill round-trip: with a region active C-w kills mark..point, and the killed
;; text is yankable with C-y -> the buffer is restored.
(test-equal "C-w kills the region; C-y yanks it back"
  "hello"
  (editor-simulate "> "
    (string-append "hello"
                   (string (ctrl #\a))   ; to start
                   c-space               ; set mark at 0
                   (string (ctrl #\e))   ; point to end -> region "hello"
                   (string (ctrl #\w))   ; kill the region (buffer now empty)
                   (string (ctrl #\y))   ; yank it back
                   (string submit))))

;; No region active: C-w preserves its historical backward-kill-word behaviour.
(test-equal "C-w with no region still backward-kills a word"
  "foo "
  (editor-simulate "> "
    (string-append "foo bar"
                   (string (ctrl #\w))   ; no mark set -> backward-kill-word
                   (string submit))))

;; The three keys are bound in BOTH the insert and normal keymaps.
(test-assert "C-Space (NUL) is bound in both keymaps"
  (and (procedure? (keymap-lookup editor-insert-keymap (make-key-event 'char (integer->char 0) 0)))
       (procedure? (keymap-lookup editor-normal-keymap (make-key-event 'char (integer->char 0) 0)))))

(test-assert "M-w is bound in both keymaps"
  (and (procedure? (keymap-lookup editor-insert-keymap (make-key-event 'meta #\w 0)))
       (procedure? (keymap-lookup editor-normal-keymap (make-key-event 'meta #\w 0)))))

(test-assert "C-w is bound in both keymaps"
  (and (procedure? (keymap-lookup editor-insert-keymap (make-key-event 'ctrl #\w 0)))
       (procedure? (keymap-lookup editor-normal-keymap (make-key-event 'ctrl #\w 0)))))

(test-end)
