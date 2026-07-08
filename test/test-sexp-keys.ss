;;; test/test-sexp-keys.ss -- Coverage for the configurable structural
;;; s-expression keybinding layer: the five key parameters hold the decoder-
;;; correct Lispy / lispyville (Doom-Emacs) defaults, all five are bound in both
;;; the insert (emacs) and normal (vi) keymaps out of the box, and bind-sexp-keys!
;;; reads the parameters at bind time so an init override rebinds the command.
;;; Pure unit assertions over the parameters and keymaps -- no terminal, no PTY,
;;; so the suite can never block.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor editor)
        (hafod editor keymap)
        (hafod editor input-decode))

(test-begin "sexp-keys")

;; --- 1. Default encodings match the terminal decoder --------------------
;; C-M-SPC = ESC+NUL -> (meta #\nul 0); M-SPC = ESC+space -> (meta #\space 0);
;; C-M-t = ESC+C-t -> (ctrl #\t MOD_ALT); M-j / M-k = (meta #\j 0) / (meta #\k 0).
(test-assert "sexp-expand-key defaults to C-M-SPC (meta nul)"
  (key-event=? (sexp-expand-key) (make-key-event 'meta (integer->char 0) 0)))
(test-assert "sexp-contract-key defaults to M-SPC (meta space)"
  (key-event=? (sexp-contract-key) (make-key-event 'meta #\space 0)))
(test-assert "sexp-transpose-key defaults to C-M-t (ctrl t + alt)"
  (key-event=? (sexp-transpose-key) (make-key-event 'ctrl #\t MOD_ALT)))
(test-assert "sexp-drag-fwd-key defaults to M-j (meta j)"
  (key-event=? (sexp-drag-fwd-key) (make-key-event 'meta #\j 0)))
(test-assert "sexp-drag-back-key defaults to M-k (meta k)"
  (key-event=? (sexp-drag-back-key) (make-key-event 'meta #\k 0)))

;; --- 2. Default-on in BOTH keymaps --------------------------------------
;; Each default key resolves to a command procedure in the insert (emacs) and
;; the normal (vi) keymap -- the structural commands are reachable out of the box
;; in both, with no configuration.
(for-each
  (lambda (named-key)
    (let ([name (car named-key)] [key (cdr named-key)])
      (test-assert (string-append name " resolves to a command in the insert keymap")
        (procedure? (keymap-lookup editor-insert-keymap key)))
      (test-assert (string-append name " resolves to a command in the normal keymap")
        (procedure? (keymap-lookup editor-normal-keymap key)))))
  (list (cons "expand (C-M-SPC)"   (sexp-expand-key))
        (cons "contract (M-SPC)"   (sexp-contract-key))
        (cons "transpose (C-M-t)"  (sexp-transpose-key))
        (cons "drag-forward (M-j)" (sexp-drag-fwd-key))
        (cons "drag-back (M-k)"    (sexp-drag-back-key))))

;; --- 3. Configurable indirection ----------------------------------------
;; bind-sexp-keys! reads the parameter at bind time, so setting the parameter
;; and re-invoking installs the NEW key.  A fresh keymap keeps the module
;; keymaps untouched by the test.
(test-assert "an init override rebinds transpose to the new key on a fresh keymap"
  (let ([km (make-keymap)])
    (parameterize ([sexp-transpose-key (make-key-event 'ctrl #\y MOD_ALT)])
      (bind-sexp-keys! km))
    (procedure? (keymap-lookup km (make-key-event 'ctrl #\y MOD_ALT)))))

;; The read is the overridden value, not a hardcoded literal: after the override
;; the fresh keymap does NOT carry the default C-M-t.
(test-assert "the override does not bind the default key -- proof of no literal"
  (let ([km (make-keymap)])
    (parameterize ([sexp-transpose-key (make-key-event 'ctrl #\y MOD_ALT)])
      (bind-sexp-keys! km))
    (not (keymap-lookup km (make-key-event 'ctrl #\t MOD_ALT)))))

;; The override is dynamically scoped: outside the parameterize the parameter
;; holds its default again, so the module keymaps built at load are unaffected.
(test-assert "the default is restored after the override extent"
  (key-event=? (sexp-transpose-key) (make-key-event 'ctrl #\t MOD_ALT)))

(test-end)
