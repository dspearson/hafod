(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor input-decode)
        (hafod editor keymap)
        (hafod editor editor))

(test-begin "Keymap layers")

;;; keymap-unbind! removes a single-key binding

(let ([km (make-keymap)]
      [cmd-a (lambda () 'a)])
  (keymap-bind! km (list (make-key-event 'ctrl #\a 0)) cmd-a)
  (test-assert "binding exists before unbind"
    (eq? (keymap-lookup km (make-key-event 'ctrl #\a 0)) cmd-a))
  (keymap-unbind! km (list (make-key-event 'ctrl #\a 0)))
  (test-assert "binding removed after unbind"
    (not (keymap-lookup km (make-key-event 'ctrl #\a 0)))))

;;; keymap-unbind! on non-existent binding is a no-op

(let ([km (make-keymap)])
  (keymap-unbind! km (list (make-key-event 'ctrl #\z 0)))
  (test-assert "unbind non-existent is no-op"
    (not (keymap-lookup km (make-key-event 'ctrl #\z 0)))))

;;; keymap-unbind! does not affect other bindings

(let ([km (make-keymap)]
      [cmd-a (lambda () 'a)]
      [cmd-b (lambda () 'b)])
  (keymap-bind! km (list (make-key-event 'ctrl #\a 0)) cmd-a)
  (keymap-bind! km (list (make-key-event 'ctrl #\b 0)) cmd-b)
  (keymap-unbind! km (list (make-key-event 'ctrl #\a 0)))
  (test-assert "C-a removed"
    (not (keymap-lookup km (make-key-event 'ctrl #\a 0))))
  (test-assert "C-b still present"
    (eq? (keymap-lookup km (make-key-event 'ctrl #\b 0)) cmd-b)))

;;; After bind-paredit-keys!, paredit key sequences resolve to commands

(let ([km (make-keymap)])
  (bind-paredit-keys! km)
  ;; C-M-f should be bound to cmd-forward-sexp
  (test-assert "C-M-f bound after bind-paredit-keys!"
    (procedure? (keymap-lookup km (make-key-event 'ctrl #\f MOD_ALT))))
  ;; M-( should be bound to cmd-wrap-round
  (test-assert "M-( bound after bind-paredit-keys!"
    (procedure? (keymap-lookup km (make-key-event 'meta #\( 0)))))

;;; After unbind-paredit-keys!, paredit key sequences return #f

(let ([km (make-keymap)])
  (bind-paredit-keys! km)
  (unbind-paredit-keys! km)
  (test-assert "C-M-f unbound after unbind-paredit-keys!"
    (not (keymap-lookup km (make-key-event 'ctrl #\f MOD_ALT))))
  (test-assert "M-( unbound after unbind-paredit-keys!"
    (not (keymap-lookup km (make-key-event 'meta #\( 0))))
  (test-assert "M-s unbound after unbind-paredit-keys!"
    (not (keymap-lookup km (make-key-event 'meta #\s 0)))))

;;; Base bindings remain after unbind-paredit-keys!

(let ([km (make-keymap)])
  (bind-base-keys! km)
  (bind-paredit-keys! km)
  (unbind-paredit-keys! km)
  ;; Base bindings should still work
  (test-assert "C-a still bound (base)"
    (procedure? (keymap-lookup km (make-key-event 'ctrl #\a 0))))
  (test-assert "C-e still bound (base)"
    (procedure? (keymap-lookup km (make-key-event 'ctrl #\e 0))))
  (test-assert "C-f still bound (base)"
    (procedure? (keymap-lookup km (make-key-event 'ctrl #\f 0))))
  (test-assert "C-b still bound (base)"
    (procedure? (keymap-lookup km (make-key-event 'ctrl #\b 0)))))

;;; toggle-paredit! flips paredit-enabled? state

(test-assert "paredit enabled by default"
  (paredit-enabled?))

;; Ensure paredit is enabled before toggle test
(enable-paredit!)
(toggle-paredit!)
(test-assert "paredit disabled after toggle"
  (not (paredit-enabled?)))
(toggle-paredit!)
(test-assert "paredit re-enabled after second toggle"
  (paredit-enabled?))

;;; paredit-enabled? returns correct boolean

(enable-paredit!)
(test-assert "paredit-enabled? returns #t when enabled"
  (eq? (paredit-enabled?) #t))
(disable-paredit!)
(test-assert "paredit-enabled? returns #f when disabled"
  (eq? (paredit-enabled?) #f))

;; Re-enable for subsequent tests/normal state
(enable-paredit!)

(test-end)
