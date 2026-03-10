;;; (hafod config) -- User configuration module: XDG discovery, config loading, key binding API
;;; Provides xdg-config-home, hafod-config-dir, hafod-init-file, load-config-file,
;;; set-prompt!, bind-key!, and parse-key-description.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod config)
  (export xdg-config-home hafod-config-dir hafod-init-file
          load-config-file
          set-prompt!
          bind-key!
          parse-key-description)
  (import (chezscheme)
          (hafod editor input-decode)
          (hafod editor keymap)
          (hafod editor editor)
          (hafod interactive)
          (hafod user-group))

  ;; ======================================================================
  ;; XDG config discovery
  ;; ======================================================================

  (define (xdg-config-home)
    (let ([xdg (getenv "XDG_CONFIG_HOME")])
      (if (and xdg (> (string-length xdg) 0))
          xdg
          (string-append (home-directory) "/.config"))))

  (define (hafod-config-dir)
    (string-append (xdg-config-home) "/hafod"))

  (define (hafod-init-file)
    (string-append (hafod-config-dir) "/init.ss"))

  ;; ======================================================================
  ;; Config file loading
  ;; ======================================================================

  (define (load-config-file path)
    (when (file-exists? path)
      (guard (e [#t
                 (display "Error loading " (current-error-port))
                 (display path (current-error-port))
                 (display ": " (current-error-port))
                 (display-condition e (current-error-port))
                 (newline (current-error-port))])
        (load path (lambda (x) (eval x (interaction-environment)))))))

  ;; ======================================================================
  ;; set-prompt!
  ;; ======================================================================

  (define (set-prompt! str)
    (repl-prompt-string str))

  ;; ======================================================================
  ;; parse-key-description -- Emacs-style key description parser
  ;; ======================================================================

  ;; Split string on spaces, returning list of substrings
  (define (string-split-spaces str)
    (let ([len (string-length str)])
      (let loop ([i 0] [start 0] [acc '()])
        (cond
          [(= i len)
           (reverse (if (> i start)
                        (cons (substring str start i) acc)
                        acc))]
          [(char=? (string-ref str i) #\space)
           (loop (+ i 1) (+ i 1)
                 (if (> i start)
                     (cons (substring str start i) acc)
                     acc))]
          [else (loop (+ i 1) start acc)]))))

  ;; Special key name table
  (define special-key-names
    '(("return" . return)
      ("tab" . tab)
      ("up" . up)
      ("down" . down)
      ("left" . left)
      ("right" . right)
      ("home" . home)
      ("end" . end)
      ("delete" . delete)
      ("backspace" . backspace)
      ("escape" . escape)))

  ;; Parse a single key spec (e.g., "C-a", "M-f", "C-M-x", "<return>")
  (define (parse-single-key spec)
    (let ([len (string-length spec)])
      (let loop ([i 0] [ctrl? #f] [meta? #f])
        (cond
          ;; Check for C- prefix
          [(and (<= (+ i 2) len)
                (char=? (string-ref spec i) #\C)
                (char=? (string-ref spec (+ i 1)) #\-))
           (loop (+ i 2) #t meta?)]
          ;; Check for M- prefix
          [(and (<= (+ i 2) len)
                (char=? (string-ref spec i) #\M)
                (char=? (string-ref spec (+ i 1)) #\-))
           (loop (+ i 2) ctrl? #t)]
          ;; Check for <name> special key
          [(and (> len i)
                (char=? (string-ref spec i) #\<)
                (> len (+ i 1))
                (char=? (string-ref spec (- len 1)) #\>))
           (let* ([name (substring spec (+ i 1) (- len 1))]
                  [entry (assoc name special-key-names)])
             (if entry
                 (make-key-event 'special (cdr entry)
                                 (bitwise-ior (if ctrl? MOD_CTRL 0)
                                              (if meta? MOD_ALT 0)))
                 (error 'parse-key-description "unknown special key" name)))]
          ;; Single character remaining
          [(= (- len i) 1)
           (let ([ch (string-ref spec i)])
             (cond
               [ctrl?
                (make-key-event 'ctrl ch (if meta? MOD_ALT 0))]
               [meta?
                (make-key-event 'meta ch 0)]
               [else
                (make-key-event 'char ch 0)]))]
          [else
           (error 'parse-key-description "invalid key spec" spec)]))))

  (define (parse-key-description desc)
    (map parse-single-key (string-split-spaces desc)))

  ;; ======================================================================
  ;; bind-key!
  ;; ======================================================================

  (define (bind-key! key-desc command . args)
    (let ([km (if (null? args) editor-insert-keymap (car args))])
      (keymap-bind! km (parse-key-description key-desc) command)))

) ;; end library
