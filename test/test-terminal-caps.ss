;;; test/test-terminal-caps.ss -- Unit suite for the terminal capability predicates.
;;; Covers: predicate purity; the defensive resolver (a string port or a non-tty
;;; fd yields #f and never raises); and, on a real tty (a pty slave), the live
;;; TERM/TERM=dumb/unset gating of ansi-ok? and the NO_COLOR-by-presence gating of
;;; colour-ok?.  No real terminal is required: the pty slave is a genuine tty, and
;;; TERM/NO_COLOR are toggled with with-env*.
;;;
;;; Note on with-env*: its deltas are synced to the OS via posix-setenv, which
;;; rejects a non-string value, so a delta must NEVER carry #f.  To establish an
;;; UNSET baseline for a variable, use (setenv NAME #f) -- which calls
;;; posix-unsetenv -- and only ever hand with-env* string values.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod terminal-caps)
        (only (hafod environment) with-env* getenv setenv)
        (only (hafod pty) open-pty)
        (only (hafod fd-ports) open-file open/read close fdes->outport)
        (only (hafod posix) posix-open O_WRONLY)
        (only (hafod editor render) tokenize display-colourised)
        (only (hafod editor editor) read-expression)
        (only (hafod editor help) show-keybindings)
        (except (chezscheme) getenv))

(test-begin "terminal-caps")

;; ======================================================================
;; Purity: both predicates are ordinary procedures (no cached global).
;; ======================================================================

(test-assert "ansi-ok? is a procedure" (procedure? ansi-ok?))
(test-assert "colour-ok? is a procedure" (procedure? colour-ok?))

;; ======================================================================
;; The presence contract colour-ok? relies on: an empty NO_COLOR is a
;; truthy string (set), not #f -- so presence, not value, disables colour.
;; ======================================================================

(test-assert "getenv reports an empty NO_COLOR as a truthy \"\" (presence, not value)"
  (with-env* '(("NO_COLOR" . ""))
    (lambda () (equal? "" (getenv "NO_COLOR")))))

;; ======================================================================
;; Defensive resolution: a non-fd/non-fdport target is "not a terminal"
;; and yields #f rather than raising.  Run under a capable TERM so the #f
;; is provably the resolver's doing, not a missing/dumb TERM.  A passing
;; test-assert proves no exception escaped (the runner turns a raise into
;; a failure).
;; ======================================================================

(with-env* '(("TERM" . "xterm"))
  (lambda ()
    (test-assert "ansi-ok? on a string port is #f (no raise)"
      (not (ansi-ok? (open-output-string))))
    (test-assert "colour-ok? on a string port is #f (no raise)"
      (not (colour-ok? (open-output-string))))
    (test-assert "ansi-ok? on a non-tty /dev/null fd is #f"
      (let ([devnull (fdes->outport (posix-open "/dev/null" O_WRONLY 0))])
        (let ([r (not (ansi-ok? devnull))]) (close devnull) r)))
    (test-assert "colour-ok? on a non-tty /dev/null fd is #f"
      (let ([devnull (fdes->outport (posix-open "/dev/null" O_WRONLY 0))])
        (let ([r (not (colour-ok? devnull))]) (close devnull) r)))))

;; ======================================================================
;; On a real tty (a pty slave): TERM and NO_COLOR gate live.
;; Establish an unset baseline first, then flip each variable with a
;; string-valued with-env* delta.
;; ======================================================================

(let-values ([(master slave-name) (open-pty)])
  (let ([slave (open-file slave-name open/read)])
    (setenv "TERM" #f)       ; unset (posix-unsetenv) -- never via a with-env* #f
    (setenv "NO_COLOR" #f)   ; unset baseline for the colour cases below

    (test-assert "ansi-ok? on a tty is #f when TERM is unset"
      (not (ansi-ok? slave)))
    (test-assert "ansi-ok? on a tty is #t under TERM=xterm"
      (with-env* '(("TERM" . "xterm")) (lambda () (ansi-ok? slave))))
    (test-assert "ansi-ok? on a tty is #f under TERM=dumb"
      (with-env* '(("TERM" . "dumb")) (lambda () (not (ansi-ok? slave)))))

    (test-assert "colour-ok? on a tty is #t under TERM=xterm with NO_COLOR unset"
      (with-env* '(("TERM" . "xterm")) (lambda () (colour-ok? slave))))
    (test-assert "colour-ok? on a tty is #f under NO_COLOR=\"\" (empty-set presence)"
      (with-env* '(("TERM" . "xterm") ("NO_COLOR" . ""))
        (lambda () (not (colour-ok? slave)))))
    (test-assert "colour-ok? on a tty is #f under NO_COLOR=1 (presence)"
      (with-env* '(("TERM" . "xterm") ("NO_COLOR" . "1"))
        (lambda () (not (colour-ok? slave)))))

    (close slave)
    (close master)))

;; ======================================================================
;; Emitter gating at the source: display-colourised threads a trailing
;; colour? that both fully suppresses and fully restores the SGR it writes.
;; This is provable without a real terminal -- a string port is a non-tty
;; sink and the emitter honours the boolean directly, so #f is the headline
;; zero-escape case and #t (or the four-argument default) is the positive
;; marker that colour still renders unchanged.
;; ======================================================================

;; A deliberately colourful input: a paren, an identifier, a string, a
;; boolean and a number all take a coloured branch when colour? is #t.
(define emitter-sample "(a \"s\" #t 1)")

;; Render emitter-sample through display-colourised and return the bytes.
(define (render-colourised colour?)
  (let ([sp (open-output-string)])
    (display-colourised sp emitter-sample (tokenize emitter-sample) -1 colour?)
    (get-output-string sp)))

;; The four-argument call exercises the default colour? (#t).
(define (render-colourised/default)
  (let ([sp (open-output-string)])
    (display-colourised sp emitter-sample (tokenize emitter-sample) -1)
    (get-output-string sp)))

;; Does the string carry any ESC (\x1b) byte at all?
(define (has-esc? s)
  (let ([n (string-length s)])
    (let scan ([i 0])
      (and (< i n)
           (or (char=? (string-ref s i) #\x1b)
               (scan (+ i 1)))))))

;; Does the string carry an ESC '[' (the CSI introducer every SGR uses)?
(define (has-csi? s)
  (let ([n (string-length s)])
    (let scan ([i 0])
      (and (< (+ i 1) n)
           (or (and (char=? (string-ref s i) #\x1b)
                    (char=? (string-ref s (+ i 1)) #\[))
               (scan (+ i 1)))))))

(test-assert "display-colourised with colour? #f emits zero ESC bytes"
  (not (has-esc? (render-colourised #f))))

(test-assert "display-colourised with colour? #t still emits SGR (ESC [)"
  (has-csi? (render-colourised #t)))

(test-assert "display-colourised defaults colour? to #t (four-argument call emits SGR)"
  (has-csi? (render-colourised/default)))

;; ======================================================================
;; Editor-control gating: the line editor's own escapes (entry cursor
;; shape/colour, clear-screen, completion cursor) gate on the render
;; target just as render.ss does.  Drive the real read-expression with a
;; deterministic, bounded key sequence -- Ctrl-L (0x0c, clear-screen)
;; followed by end-of-input, which submits -- and prove the escapes vanish
;; on a non-terminal out-port and remain on a capable one.  A string input
;; port makes the drive hang-free: read-key-event returns eof at exhaustion
;; and the editor finishes at once (no unbounded read on a live terminal).
;;
;; The bracketed-paste pair writes to the console-output-port, not this
;; injected out-port, so it is out of this capture by construction; its
;; production suppression rides on the piped-stdout end-to-end check.
;; ======================================================================

;; Ctrl-L clears the screen; end-of-input then submits.
(define editor-drive-keys (string #\x0c))

;; (1) Suppressed: a non-terminal out-port yields zero editor escapes.  Run
;; under a capable TERM so the absence is provably the out-port's doing, not
;; a missing/dumb TERM.
(test-assert "editor control escapes suppress to a non-terminal out-port (zero ESC)"
  (with-env* '(("TERM" . "xterm"))
    (lambda ()
      (let ([out (open-output-string)])
        (read-expression "> " (open-input-string editor-drive-keys) out)
        (not (has-esc? (get-output-string out)))))))

;; (2) Present: a capable pty-slave out-port still emits the cursor/clear
;; escapes.  The entry cursor escape is the first byte written, so the first
;; read from the master is ESC; a bounded loop caps any residual stall and
;; any read error or EOF yields #f (never an unbounded wait).
(test-assert "editor control escapes emit on a capable pty-slave target (ESC present)"
  (let-values ([(master slave-name) (open-pty)])
    (setenv "NO_COLOR" #f)   ; unset baseline -- never via a with-env* #f delta
    (let ([slave-out (fdes->outport (posix-open slave-name O_WRONLY 0))])
      (let ([result
             (with-env* '(("TERM" . "xterm"))
               (lambda ()
                 (read-expression "> " (open-input-string editor-drive-keys) slave-out)
                 (flush-output-port slave-out)
                 (guard (e [#t #f])
                   (let loop ([count 0])
                     (if (>= count 64)
                         #f
                         (let ([ch (read-char master)])
                           (cond
                             [(eof-object? ch) #f]
                             [(char=? ch #\x1b) #t]
                             [else (loop (+ count 1))])))))))])
        (close slave-out)
        (close master)
        result))))

;; The keybinding cheatsheet must not leak escapes to a non-terminal sink.
;; with-output-to-string rebinds current-output-port to a string port (not a
;; tty), so colour-ok?/ansi-ok? are #f and the whole reference prints plain.
(test-assert "show-keybindings emits no escape bytes to a non-terminal sink"
  (let ([out (with-output-to-string (lambda () (show-keybindings)))])
    (and (> (string-length out) 0)
         (not (let has-esc? ([i 0])
                (cond
                  [(>= i (string-length out)) #f]
                  [(char=? (string-ref out i) #\x1b) #t]
                  [else (has-esc? (+ i 1))]))))))

(test-end)
