;;; test/test-evil-default-on.ss -- The whole editing surface is on by default,
;;; with no configuration.  Two parts, both PTY-free:
;;;
;;;   Part A builds FRESH keymaps -- (make-normal-keymap) and (make-insert-keymap)
;;;   -- and asserts their bindings directly, proving the features are installed at
;;;   construction rather than by some mutated global: paste (p / P), simple
;;;   editing (x) and insert entry (i / a) are bound in the normal keymap; the
;;;   structural transpose key is bound in BOTH keymaps (so it works in normal and
;;;   insert alike); the insert keymap maps Escape to cmd-enter-normal-mode; and
;;;   the one deliberate opt-in-off -- the text mode indicator -- is off, since the
;;;   mode is already shown through the cursor shape.
;;;
;;;   Part B drives the real dispatch with NO parameter set beforehand: a config-
;;;   free read-expression run shows the editor starts in insert mode (typing
;;;   inserts, then submits), and a fresh insert->Escape->normal drive reaches a
;;;   structural surround edit, a . repeat, and a named-register yank+paste -- so
;;;   from a fresh session Escape enters a fully-featured normal/visual mode and
;;;   every feature takes effect out of the box.
;;;
;;; A note on the Escape drives: the byte decoder tells a lone Escape from an Alt
;;; sequence with char-ready?, which on a bounded string port is always true mid-
;;; stream -- so a single read-expression script cannot enter normal mode with an
;;; Escape followed by more keys (it would decode as Meta).  editor-drive-keys! --
;;; the white-box harness that runs the SAME recording boundary and dispatch the
;;; interactive loop runs -- sidesteps this by feeding each keystroke group as its
;;; own bounded port, so an isolated Escape sits at end-of-port and decodes to
;;; 'escape exactly as a real terminal delivers it.  Every port is bounded, so no
;;; read can block and the suite cannot hang.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor keymap)
        (hafod editor input-decode)
        (hafod editor gap-buffer)
        (hafod editor kill-ring)
        ;; Importing the editor library installs vi.ss's procedure hooks at load,
        ;; so a driven vi command runs without a null-hook error, and gives us the
        ;; fresh keymap constructors, the paste commands and the drive harness.
        (hafod editor editor))

;; The single Escape byte (isolated in its own chunk it decodes to 'escape).
(define ESC (string (integer->char #x1b)))

;; Construct a fresh insert-mode editor-state around an empty buffer -- exactly as
;; read-expression does at the start of a line, with nothing configured.
(define (fresh-insert-state)
  (let ([gb (make-gap-buffer)])
    (cons gb
          (make-editor-state gb (make-kill-ring) "> "
                             (open-output-string) 0 #f #f 'insert #f))))

(test-begin "evil-default-on")

;; ============================================================================
;; Part A -- fresh-keymap binding assertions (default-on by construction)
;; ============================================================================

(define nkm (make-normal-keymap))
(define ikm (make-insert-keymap))

;; Registers paste is on: p pastes after, P pastes before -- straight off a fresh
;; normal keymap, no configuration.
(test-assert "a fresh normal keymap binds p to paste-after"
  (eq? (keymap-lookup nkm (make-key-event 'char #\p 0)) cmd-paste-after))
(test-assert "a fresh normal keymap binds P to paste-before"
  (eq? (keymap-lookup nkm (make-key-event 'char #\P 0)) cmd-paste-before))

;; Simple editing and insert entry are on: x deletes, i / a enter insert.
(test-assert "a fresh normal keymap binds x to a command"
  (procedure? (keymap-lookup nkm (make-key-event 'char #\x 0))))
(test-assert "a fresh normal keymap binds i (insert entry) to a command"
  (procedure? (keymap-lookup nkm (make-key-event 'char #\i 0))))
(test-assert "a fresh normal keymap binds a (insert entry) to a command"
  (procedure? (keymap-lookup nkm (make-key-event 'char #\a 0))))

;; The structural transpose is bound in BOTH keymaps, so structural editing works
;; in normal AND insert with no opt-in.
(test-assert "a fresh normal keymap binds the structural transpose key"
  (procedure? (keymap-lookup nkm (sexp-transpose-key))))
(test-assert "a fresh insert keymap binds the structural transpose key too"
  (procedure? (keymap-lookup ikm (sexp-transpose-key))))

;; Escape from insert reaches the fully-featured normal/visual mode.
(test-assert "a fresh insert keymap maps Escape to cmd-enter-normal-mode"
  (eq? (keymap-lookup ikm (make-key-event 'special 'escape 0))
       cmd-enter-normal-mode))

;; The one deliberate opt-in-off holds with no configuration: the text mode
;; indicator is off (the mode is shown through the cursor shape instead).
(test-assert "the text mode indicator is off by default (the one opt-in-off)"
  (not (show-mode-indicator?)))

;; ============================================================================
;; Part B -- fresh-REPL end-to-end reachability, nothing parameterised
;; ============================================================================

;; B1: a config-free read-expression run starts in insert mode, so typing inserts
;; and Return submits the typed text -- the editor is insert-default out of the
;; box, driven through the real main entry point.
(test-equal "read-expression is insert-default: typed text inserts and submits"
  "hello world"
  (read-expression "> " (open-input-string "hello world\r") (open-output-string)))

;; B2: a fresh insert->Escape->normal drive reaches a structural surround edit AND
;; a . repeat with no configuration.  Type "aa bb" (insert-default), Escape to a
;; fully-featured normal mode, move to column 0, surround the inner word with
;; parens (ysiw(, its operands inline-read from the same bounded port), move to
;; end of line and . to repeat the surround on the next word -- so surround and
;; dot-repeat are both reachable and take effect from a fresh session.
(let* ([p (fresh-insert-state)]
       [gb (car p)]
       [es (cdr p)])
  (editor-drive-keys! es (list "aa bb" ESC "0" "ysiw(" "$" "."))
  (test-equal "insert->Escape->normal reaches surround AND dot-repeat, no config"
    "( aa ) ( bb )"
    (gap-buffer->string gb)))

;; B3: a fresh insert->Escape->normal drive reaches a named-register yank+paste
;; with no configuration.  Type "zz" (insert-default), Escape to normal, yank the
;; line into the named register a ("ayy), re-select register a ("a) and paste it
;; after the cursor -- inserting the register's text back into the line.  ("zz"
;; makes the paste result cursor-independent: inserting "zz" anywhere into "zz"
;; yields "zzzz".)  So the named registers are on by default and reachable end to
;; end from a fresh session.
(let* ([p (fresh-insert-state)]
       [gb (car p)]
       [es (cdr p)])
  (editor-drive-keys! es (list "zz" ESC "\"ayy" "\"a"))
  (cmd-paste-after es)
  (test-equal "insert->Escape->normal reaches a named-register yank+paste, no config"
    "zzzz"
    (gap-buffer->string gb)))

(test-end)
