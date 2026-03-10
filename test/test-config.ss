#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod config)
        (hafod editor input-decode)
        (hafod interactive))

(test-begin "Config Module")

;; === XDG config discovery ===

(test-assert "xdg-config-home returns XDG_CONFIG_HOME when set"
  (let ([old (getenv "XDG_CONFIG_HOME")])
    (putenv "XDG_CONFIG_HOME" "/tmp/test-xdg")
    (let ([result (xdg-config-home)])
      (if old (putenv "XDG_CONFIG_HOME" old) (putenv "XDG_CONFIG_HOME" ""))
      (string=? result "/tmp/test-xdg"))))

(test-assert "xdg-config-home returns HOME/.config when XDG_CONFIG_HOME unset"
  (let ([old (getenv "XDG_CONFIG_HOME")])
    (putenv "XDG_CONFIG_HOME" "")
    (let ([result (xdg-config-home)])
      (when old (putenv "XDG_CONFIG_HOME" old))
      (let ([home (getenv "HOME")])
        (string=? result (string-append home "/.config"))))))

(test-assert "hafod-config-dir returns xdg-config-home/hafod"
  (let ([old (getenv "XDG_CONFIG_HOME")])
    (putenv "XDG_CONFIG_HOME" "/tmp/test-xdg")
    (let ([result (hafod-config-dir)])
      (if old (putenv "XDG_CONFIG_HOME" old) (putenv "XDG_CONFIG_HOME" ""))
      (string=? result "/tmp/test-xdg/hafod"))))

(test-assert "hafod-init-file returns xdg-config-home/hafod/init.ss"
  (let ([old (getenv "XDG_CONFIG_HOME")])
    (putenv "XDG_CONFIG_HOME" "/tmp/test-xdg")
    (let ([result (hafod-init-file)])
      (if old (putenv "XDG_CONFIG_HOME" old) (putenv "XDG_CONFIG_HOME" ""))
      (string=? result "/tmp/test-xdg/hafod/init.ss"))))

;; === parse-key-description ===

(test-assert "parse-key-description C-a -> ctrl key-event"
  (let ([keys (parse-key-description "C-a")])
    (and (= (length keys) 1)
         (eq? (key-event-type (car keys)) 'ctrl)
         (char=? (key-event-value (car keys)) #\a)
         (= (key-event-mods (car keys)) 0))))

(test-assert "parse-key-description M-f -> meta key-event"
  (let ([keys (parse-key-description "M-f")])
    (and (= (length keys) 1)
         (eq? (key-event-type (car keys)) 'meta)
         (char=? (key-event-value (car keys)) #\f)
         (= (key-event-mods (car keys)) 0))))

(test-assert "parse-key-description C-M-f -> ctrl key-event with MOD_ALT"
  (let ([keys (parse-key-description "C-M-f")])
    (and (= (length keys) 1)
         (eq? (key-event-type (car keys)) 'ctrl)
         (char=? (key-event-value (car keys)) #\f)
         (= (key-event-mods (car keys)) MOD_ALT))))

(test-assert "parse-key-description C-x C-s -> two key-events"
  (let ([keys (parse-key-description "C-x C-s")])
    (and (= (length keys) 2)
         (eq? (key-event-type (car keys)) 'ctrl)
         (char=? (key-event-value (car keys)) #\x)
         (eq? (key-event-type (cadr keys)) 'ctrl)
         (char=? (key-event-value (cadr keys)) #\s))))

(test-assert "parse-key-description <return> -> special key-event"
  (let ([keys (parse-key-description "<return>")])
    (and (= (length keys) 1)
         (eq? (key-event-type (car keys)) 'special)
         (eq? (key-event-value (car keys)) 'return)
         (= (key-event-mods (car keys)) 0))))

(test-assert "parse-key-description <up> -> special key-event"
  (let ([keys (parse-key-description "<up>")])
    (and (= (length keys) 1)
         (eq? (key-event-type (car keys)) 'special)
         (eq? (key-event-value (car keys)) 'up)
         (= (key-event-mods (car keys)) 0))))

;; === set-prompt! ===

(test-assert "set-prompt! changes repl-prompt-string"
  (let ([old (repl-prompt-string)])
    (set-prompt! "test> ")
    (let ([result (string=? (repl-prompt-string) "test> ")])
      (repl-prompt-string old)
      result)))

;; === load-config-file ===

(test-assert "load-config-file on non-existent file does nothing"
  (begin
    (load-config-file "/tmp/hafod-test-nonexistent-file-abc123.ss")
    #t))

(test-assert "load-config-file on file with syntax error prints error but does not raise"
  (let ([tmp "/tmp/hafod-test-bad-config.ss"])
    (call-with-output-file tmp
      (lambda (p) (display "(define x (" p))
      'replace)
    (let ([result (guard (e [#t #f])
                    (load-config-file tmp)
                    #t)])
      (delete-file tmp)
      result)))

(test-end)
