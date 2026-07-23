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
        ;; A bare (hafod terminal-caps) import also surfaces the new
        ;; assume-terminal-caps override parameter exercised below.
        (hafod terminal-caps)
        (only (hafod environment) with-env* getenv setenv)
        (only (hafod pty) open-pty)
        (only (hafod fd-ports) open-file open/read close fdes->outport)
        (only (hafod posix) posix-open O_WRONLY)
        (only (hafod editor render) tokenize display-colourised render-line)
        (only (hafod editor gap-buffer) make-gap-buffer gap-buffer-set-from-string!)
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
;;
;; Pitfall pin: colour-ok? now reads CLICOLOR_FORCE, and an ambient
;; CLICOLOR_FORCE=1 would force colour on a plain (non-tty) string port,
;; flipping the "colour-ok? on a string port is #f" assert below.  Establish a
;; fresh CLICOLOR_FORCE-unset baseline first (setenv #f, never a with-env* #f).
;; ======================================================================

(setenv "CLICOLOR_FORCE" #f)

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

;; ======================================================================
;; The assume-terminal-caps override seam: a default-off parameter that
;; forces the capability verdict, so the port-gated render path emits its
;; full escape stream into a plain string port with no pty and no real tty.
;; Cases (1)-(3) need no terminal at all; case (4) uses a pty slave only to
;; prove 'off wins even when the live probe would otherwise say "yes".
;; ======================================================================

;; A gap buffer pre-filled with s (cursor left at the end of the text).
(define (buffer-from s)
  (let ([gb (make-gap-buffer)])
    (gap-buffer-set-from-string! gb s)
    gb))

;; Establish an unset baseline so the forced-'on cases prove the override
;; ignores the environment (unset via setenv #f, never via a with-env* #f).
(setenv "TERM" #f)
(setenv "NO_COLOR" #f)

;; (1) DEFAULT-OFF, byte-identical: with the parameter unset, render-line into
;; a string port emits ZERO escape bytes -- exactly as before the seam existed.
;; This pins that the seam causes no behavioural change when it is unused (the
;; additive-API regression guard).
(test-assert "assume-terminal-caps unset: render-line to a string port emits zero ESC"
  (let ([sp (open-output-string)])
    (render-line sp "> " (buffer-from "(+ 1 2)") 0 80)
    (not (has-esc? (get-output-string sp)))))

;; (2) FORCED-ON predicates: 'on forces both predicates #t for any target,
;; bypassing the fd/tty/TERM probe -- with no TERM set at all, and even under a
;; set NO_COLOR (which would veto colour-ok? on the live path).
(test-assert "assume-terminal-caps 'on forces ansi-ok? #t on a string port (no TERM)"
  (parameterize ([assume-terminal-caps 'on])
    (ansi-ok? (open-output-string))))
(test-assert "assume-terminal-caps 'on forces colour-ok? #t on a string port (no TERM)"
  (parameterize ([assume-terminal-caps 'on])
    (colour-ok? (open-output-string))))
(test-assert "assume-terminal-caps 'on makes colour-ok? bypass the NO_COLOR veto"
  (with-env* '(("NO_COLOR" . "1"))
    (lambda ()
      (parameterize ([assume-terminal-caps 'on])
        (and (ansi-ok? (open-output-string))
             (colour-ok? (open-output-string)))))))

;; (3) FORCED-ON emission: inside the same override, render-line into a string
;; port now DOES emit escape bytes -- the port gate flipped, so the harness can
;; capture the full escape stream with no pty.
(test-assert "assume-terminal-caps 'on makes render-line emit ESC to a string port"
  (parameterize ([assume-terminal-caps 'on])
    (let ([sp (open-output-string)])
      (render-line sp "> " (buffer-from "(+ 1 2)") 0 80)
      (has-esc? (get-output-string sp)))))

;; (4) FORCED-OFF: 'off forces both predicates #f even on a genuine tty (a pty
;; slave) under a capable TERM -- proving the explicit-off path wins over the
;; live verdict.  No master read/drain occurs, so this stays cross-platform and
;; hang-free.
(let-values ([(master slave-name) (open-pty)])
  (let ([slave (open-file slave-name open/read)])
    (with-env* '(("TERM" . "xterm"))
      (lambda ()
        (test-assert "assume-terminal-caps 'off forces ansi-ok? #f on a real tty"
          (parameterize ([assume-terminal-caps 'off])
            (not (ansi-ok? slave))))
        (test-assert "assume-terminal-caps 'off forces colour-ok? #f on a real tty"
          (parameterize ([assume-terminal-caps 'off])
            (not (colour-ok? slave))))))
    (close slave)
    (close master)))

;; ======================================================================
;; The colour-override seam + the CLICOLOR_FORCE / NO_COLOR precedence.
;; colour-override is the colour-ONLY override a launcher --color flag sets: it
;; is consulted AFTER the assume-terminal-caps short-circuit and BEFORE the env
;; reads, and it NEVER touches ansi-ok?.  With colour-override #f the LOCKED
;; precedence holds -- an explicit override wins; then NO_COLOR (presence) beats
;; CLICOLOR_FORCE; then CLICOLOR_FORCE (set and not "0") forces; then the
;; byte-identical ansi-ok? auto path.  Proven PTY-free by parameterizing the
;; override and toggling the two env vars, exactly as the seam cases above; the
;; 'never / auto tty rows reuse the pty-slave idiom.  A fresh unset baseline for
;; all three variables first (setenv #f, never a with-env* #f delta).
;; ======================================================================

(setenv "TERM" #f)
(setenv "NO_COLOR" #f)
(setenv "CLICOLOR_FORCE" #f)

;; (1) colour-override 'always beats NO_COLOR AND a non-tty target: colour-ok? is
;; #t on a plain string port even under NO_COLOR=1 (--color=always wins over all).
(test-assert "colour-override 'always forces colour-ok? on a string port under NO_COLOR=1"
  (with-env* '(("NO_COLOR" . "1"))
    (lambda ()
      (parameterize ([colour-override 'always])
        (colour-ok? (open-output-string))))))

;; (2) Colour-only scope (authoritative): under 'always, colour-ok? flips #t on a
;; string port while ansi-ok? on the same port stays #f -- the cursor/alt-screen
;; rail is untouched, so --color=always | cat emits SGR but no cursor escapes.
(test-assert "colour-override 'always leaves ansi-ok? #f on a string port (colour-only scope)"
  (parameterize ([colour-override 'always])
    (and (colour-ok? (open-output-string))
         (not (ansi-ok? (open-output-string))))))

;; (3) colour-override 'never suppresses colour on a REAL tty (a pty slave) even
;; under TERM=xterm and CLICOLOR_FORCE=1 -- --color=never beats force + a tty --
;; while ansi-ok? on that same tty is unchanged (#t), confirming colour-only scope.
(let-values ([(master slave-name) (open-pty)])
  (let ([slave (open-file slave-name open/read)])
    (with-env* '(("TERM" . "xterm") ("CLICOLOR_FORCE" . "1"))
      (lambda ()
        (test-assert "colour-override 'never suppresses colour-ok? on a real tty (beats force + tty)"
          (parameterize ([colour-override 'never])
            (not (colour-ok? slave))))
        (test-assert "colour-override 'never leaves ansi-ok? #t on that same tty (colour-only scope)"
          (parameterize ([colour-override 'never])
            (ansi-ok? slave)))))
    (close slave)
    (close master)))

;; (4) The auto (colour-override #f) precedence matrix over NO_COLOR x
;; CLICOLOR_FORCE x {string-port, pty+xterm}.
;;
;; (4a) NO_COLOR beats CLICOLOR_FORCE (the locked accessibility-first rule):
;; NO_COLOR="" present + CLICOLOR_FORCE="1" -> colour-ok? #f, on a string port and
;; on a real tty alike.
(test-assert "auto: NO_COLOR beats CLICOLOR_FORCE on a string port"
  (with-env* '(("NO_COLOR" . "") ("CLICOLOR_FORCE" . "1"))
    (lambda () (not (colour-ok? (open-output-string))))))
(let-values ([(master slave-name) (open-pty)])
  (let ([slave (open-file slave-name open/read)])
    (test-assert "auto: NO_COLOR beats CLICOLOR_FORCE on a real tty under TERM=xterm"
      (with-env* '(("TERM" . "xterm") ("NO_COLOR" . "") ("CLICOLOR_FORCE" . "1"))
        (lambda () (not (colour-ok? slave)))))
    (close slave)
    (close master)))

;; (4b) With NO_COLOR absent, CLICOLOR_FORCE forces on a non-tty: ="1" forces
;; (string port -> #t); the value asymmetry vs NO_COLOR -- ="0" does NOT force,
;; ="" DOES (being non-"0"), the mirror of the empty-NO_COLOR presence assert.
(test-assert "auto: CLICOLOR_FORCE=1 forces colour-ok? on a string port"
  (with-env* '(("CLICOLOR_FORCE" . "1"))
    (lambda () (colour-ok? (open-output-string)))))
(test-assert "auto: CLICOLOR_FORCE=0 does NOT force (string port stays #f)"
  (with-env* '(("CLICOLOR_FORCE" . "0"))
    (lambda () (not (colour-ok? (open-output-string))))))
(test-assert "auto: CLICOLOR_FORCE=\"\" DOES force (non-\"0\" value; the asymmetry vs NO_COLOR)"
  (with-env* '(("CLICOLOR_FORCE" . ""))
    (lambda () (colour-ok? (open-output-string)))))

;; (4c) CLICOLOR_FORCE="0" declines to force and falls THROUGH to ansi-ok?, so on
;; a real tty under TERM=xterm it is #t ("0" does not suppress -- it is not
;; NO_COLOR -- it merely does not force).
(let-values ([(master slave-name) (open-pty)])
  (let ([slave (open-file slave-name open/read)])
    (test-assert "auto: CLICOLOR_FORCE=0 falls through to ansi-ok? (tty+xterm -> #t)"
      (with-env* '(("TERM" . "xterm") ("CLICOLOR_FORCE" . "0"))
        (lambda () (colour-ok? slave))))
    (close slave)
    (close master)))

;; (4d) colour-override #f with CLICOLOR_FORCE unset is byte-identical to today:
;; a string port is #f (a non-tty) and a pty slave under TERM=xterm is #t -- the
;; mirror of the pre-existing tty/NO_COLOR asserts, pinning the no-regression path.
(setenv "CLICOLOR_FORCE" #f)
(setenv "NO_COLOR" #f)
(with-env* '(("TERM" . "xterm"))
  (lambda ()
    (test-assert "auto: CLICOLOR_FORCE unset -> a string port stays #f (byte-identical)"
      (not (colour-ok? (open-output-string))))))
(let-values ([(master slave-name) (open-pty)])
  (let ([slave (open-file slave-name open/read)])
    (test-assert "auto: CLICOLOR_FORCE unset -> a pty slave under TERM=xterm is #t (byte-identical)"
      (with-env* '(("TERM" . "xterm")) (lambda () (colour-ok? slave))))
    (close slave)
    (close master)))

;; (5) The byte-level emission flip.  Under colour-override 'always, render-line
;; into a plain string port now emits CSI colour bytes (has-csi? #t) -- whereas
;; the assume-terminal-caps-unset render-line case above emits ZERO ESC -- so the
;; override flips REAL emitted colour on a non-tty.  render-line is used directly
;; here, NOT render->screen (which forces the coarse assume-terminal-caps 'on and
;; so cannot model a colour-only override).  ansi-ok? stays #f on the string port,
;; so no cursor/clear escapes are emitted: every ESC[ here introduces an SGR run.
(setenv "CLICOLOR_FORCE" #f)
(setenv "NO_COLOR" #f)
(test-assert "colour-override 'always makes render-line emit CSI colour to a string port"
  (parameterize ([colour-override 'always])
    (let ([sp (open-output-string)])
      (render-line sp "> " (buffer-from "(+ 1 2)") 0 80)
      (has-csi? (get-output-string sp)))))

;; ======================================================================
;; colour-depth: the truecolor / 256 / 16 / mono capability tier a prompt
;; segment (the exit-coloured input glyph, a per-language version segment)
;; reads to pick its SGR.  It DEFERS to colour-ok? first, so the mono verdict
;; inherits the locked NO_COLOR / colour-override / assume-terminal-caps
;; precedence rather than re-deriving it; only when colour is allowed does it
;; read COLORTERM / TERM for the depth.  A fresh unset baseline for every
;; variable it reads (setenv #f, never a with-env* #f delta); colour is forced
;; on for the non-mono rows with CLICOLOR_FORCE=1 so the depth is provable
;; PTY-free (no real tty on fd 1 needed).
;; ======================================================================

(setenv "TERM" #f)
(setenv "COLORTERM" #f)
(setenv "NO_COLOR" #f)
(setenv "CLICOLOR_FORCE" #f)

;; truecolor: COLORTERM=truecolor and =24bit are the 24-bit signal.  The
;; TERM=xterm-256color alongside proves COLORTERM is read FIRST -- a
;; COLORTERM-blind verdict would mis-report '256 here.
(test-assert "colour-depth: COLORTERM=truecolor -> 'truecolor (COLORTERM beats a 256 TERM)"
  (with-env* '(("CLICOLOR_FORCE" . "1") ("COLORTERM" . "truecolor") ("TERM" . "xterm-256color"))
    (lambda () (eq? 'truecolor (colour-depth 1)))))
(test-assert "colour-depth: COLORTERM=24bit -> 'truecolor"
  (with-env* '(("CLICOLOR_FORCE" . "1") ("COLORTERM" . "24bit"))
    (lambda () (eq? 'truecolor (colour-depth 1)))))

;; 256: a *256color* TERM with no COLORTERM.
(test-assert "colour-depth: TERM=xterm-256color (no COLORTERM) -> '256"
  (with-env* '(("CLICOLOR_FORCE" . "1") ("TERM" . "xterm-256color"))
    (lambda () (eq? '256 (colour-depth 1)))))

;; 16: a plain colour TERM, no COLORTERM, not a 256 TERM.
(test-assert "colour-depth: TERM=xterm (plain colour) -> '16"
  (with-env* '(("CLICOLOR_FORCE" . "1") ("TERM" . "xterm"))
    (lambda () (eq? '16 (colour-depth 1)))))

;; 16 fallback: colour allowed (assume-terminal-caps 'on) but NO depth signal
;; at all -> the conservative colour tier, never mono.
(test-assert "colour-depth: assume 'on, no COLORTERM/TERM -> '16 (colour allowed, no depth signal)"
  (parameterize ([assume-terminal-caps 'on])
    (eq? '16 (colour-depth 1))))

;; mono via the locked precedence -- each proves colour-depth reuses colour-ok?
;; rather than reading COLORTERM directly (a truecolor COLORTERM is present but
;; ignored because colour is not allowed; a colour-ok?-blind verdict fails).
(test-assert "colour-depth: NO_COLOR present -> 'mono (beats a truecolor COLORTERM)"
  (with-env* '(("NO_COLOR" . "1") ("COLORTERM" . "truecolor"))
    (lambda () (eq? 'mono (colour-depth 1)))))
(test-assert "colour-depth: assume-terminal-caps 'off -> 'mono (beats CLICOLOR_FORCE + COLORTERM)"
  (parameterize ([assume-terminal-caps 'off])
    (with-env* '(("CLICOLOR_FORCE" . "1") ("COLORTERM" . "truecolor"))
      (lambda () (eq? 'mono (colour-depth 1))))))
(test-assert "colour-depth: colour-override 'never -> 'mono (beats CLICOLOR_FORCE + COLORTERM)"
  (parameterize ([colour-override 'never])
    (with-env* '(("CLICOLOR_FORCE" . "1") ("COLORTERM" . "truecolor"))
      (lambda () (eq? 'mono (colour-depth 1))))))

;; ======================================================================
;; glyph-tier: the emoji / ascii verdict a segment reads to pick its glyph.
;; Emoji support CANNOT be probed, so the default is 'emoji and 'ascii is only
;; ever the fallback on a known-poor glyph terminal (TERM=linux/dumb) or an
;; explicit opt-out (the HAFOD_ASCII env var, or the glyph-tier-override seam)
;; -- a Nerd Font is NEVER assumed.  It is INDEPENDENT of colour-depth: a
;; colour tty is not necessarily a glyph tty (TERM=linux is 16-colour but a
;; tofu-prone glyph tty).  A fresh unset baseline for the variables it reads.
;; ======================================================================

(setenv "TERM" #f)
(setenv "HAFOD_ASCII" #f)

(test-assert "glyph-tier: TERM=xterm-256color -> 'emoji (the default; emoji cannot be probed)"
  (with-env* '(("TERM" . "xterm-256color")) (lambda () (eq? 'emoji (glyph-tier)))))
(test-assert "glyph-tier: TERM=linux -> 'ascii (a known-poor glyph tty)"
  (with-env* '(("TERM" . "linux")) (lambda () (eq? 'ascii (glyph-tier)))))
(test-assert "glyph-tier: TERM=dumb -> 'ascii"
  (with-env* '(("TERM" . "dumb")) (lambda () (eq? 'ascii (glyph-tier)))))
(test-assert "glyph-tier: HAFOD_ASCII set -> 'ascii (explicit opt-out, even under a capable TERM)"
  (with-env* '(("TERM" . "xterm-256color") ("HAFOD_ASCII" . "1"))
    (lambda () (eq? 'ascii (glyph-tier)))))
(test-assert "glyph-tier: glyph-tier-override 'ascii forces 'ascii (the PTY-free opt-out/test seam)"
  (with-env* '(("TERM" . "xterm-256color"))
    (lambda () (parameterize ([glyph-tier-override 'ascii]) (eq? 'ascii (glyph-tier))))))

;; Independence: TERM=linux is a 16-colour tty but a poor glyph tty -- the two
;; tiers are decided separately, so a glyph verdict that leaned on colour-depth
;; would wrongly agree here.  Colour forced on so colour-depth reads TERM=linux
;; as '16 rather than folding to 'mono on a non-tty fd 1.
(test-assert "glyph-tier independent of colour-depth: TERM=linux -> colour '16 but glyph 'ascii"
  (with-env* '(("CLICOLOR_FORCE" . "1") ("TERM" . "linux"))
    (lambda () (and (eq? '16 (colour-depth 1)) (eq? 'ascii (glyph-tier))))))

(test-end)
