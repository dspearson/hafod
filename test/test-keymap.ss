(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor input-decode)
        (hafod editor keymap))

(test-begin "Keymap")

;;; Single key binding

(let ([km (make-keymap)]
      [cmd-a (lambda () 'a-pressed)])
  (keymap-bind! km (list (make-key-event 'ctrl #\a 0)) cmd-a)
  (test-assert "bind and lookup single key"
    (eq? (keymap-lookup km (make-key-event 'ctrl #\a 0)) cmd-a)))

;;; Different keys to different commands

(let ([km (make-keymap)]
      [cmd-a (lambda () 'a)]
      [cmd-b (lambda () 'b)])
  (keymap-bind! km (list (make-key-event 'ctrl #\a 0)) cmd-a)
  (keymap-bind! km (list (make-key-event 'ctrl #\b 0)) cmd-b)
  (test-assert "C-a returns cmd-a"
    (eq? (keymap-lookup km (make-key-event 'ctrl #\a 0)) cmd-a))
  (test-assert "C-b returns cmd-b"
    (eq? (keymap-lookup km (make-key-event 'ctrl #\b 0)) cmd-b)))

;;; Unbound key returns #f

(let ([km (make-keymap)])
  (test-assert "unbound key returns #f"
    (not (keymap-lookup km (make-key-event 'ctrl #\z 0)))))

;;; Two-key chord: C-x C-e

(let ([km (make-keymap)]
      [cmd-eval (lambda () 'eval)])
  (keymap-bind! km
    (list (make-key-event 'ctrl #\x 0) (make-key-event 'ctrl #\e 0))
    cmd-eval)
  ;; First key should return a keymap (prefix)
  (let ([result (keymap-lookup km (make-key-event 'ctrl #\x 0))])
    (test-assert "C-x is prefix (returns keymap)"
      (keymap? result))
    ;; Second key in sub-keymap should return the command
    (test-assert "C-x C-e returns eval command"
      (eq? (keymap-lookup result (make-key-event 'ctrl #\e 0)) cmd-eval))))

;;; keymap-lookup-prefix

(let ([km (make-keymap)]
      [cmd-eval (lambda () 'eval)])
  (keymap-bind! km
    (list (make-key-event 'ctrl #\x 0) (make-key-event 'ctrl #\e 0))
    cmd-eval)
  (test-assert "keymap-lookup-prefix returns sub-keymap"
    (keymap? (keymap-lookup-prefix km (make-key-event 'ctrl #\x 0))))
  (test-assert "keymap-lookup-prefix on non-prefix returns #f"
    (not (keymap-lookup-prefix km (make-key-event 'ctrl #\z 0)))))

;;; Overwrite existing binding

(let ([km (make-keymap)]
      [cmd-old (lambda () 'old)]
      [cmd-new (lambda () 'new)])
  (keymap-bind! km (list (make-key-event 'ctrl #\a 0)) cmd-old)
  (keymap-bind! km (list (make-key-event 'ctrl #\a 0)) cmd-new)
  (test-assert "overwrite binding returns new command"
    (eq? (keymap-lookup km (make-key-event 'ctrl #\a 0)) cmd-new)))

;;; Multiple prefix keys coexist

(let ([km (make-keymap)]
      [cmd-eval (lambda () 'eval)]
      [cmd-save (lambda () 'save)])
  (keymap-bind! km
    (list (make-key-event 'ctrl #\x 0) (make-key-event 'ctrl #\e 0))
    cmd-eval)
  (keymap-bind! km
    (list (make-key-event 'ctrl #\x 0) (make-key-event 'ctrl #\s 0))
    cmd-save)
  (let ([sub (keymap-lookup km (make-key-event 'ctrl #\x 0))])
    (test-assert "C-x C-e -> eval"
      (eq? (keymap-lookup sub (make-key-event 'ctrl #\e 0)) cmd-eval))
    (test-assert "C-x C-s -> save"
      (eq? (keymap-lookup sub (make-key-event 'ctrl #\s 0)) cmd-save))))

(test-end)
