;;; test/test-comp-dispatch.ss -- The end-to-end proof that the editor's Tab
;;; dispatch routes a ~-token to the login completer and a dash-token on a
;;; completer-less command to the option-flag completer, and that every candidate
;;; is control-stripped before it reaches the menu.  Each case drives the real
;;; read-expression over a scripted key string under a forced capability verdict,
;;; captures the emitted escape stream to a plain string port, and asserts on the
;;; captured bytes -- exactly as the completion-overlay cycle proof does, so the
;;; drive is deterministic and returns at once (end-of-input submits).  Entirely
;;; PTY-free: string ports throughout, no terminal, no platform gate.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test vterm)
        (only (hafod editor editor) read-expression)
        (only (hafod terminal-caps) assume-terminal-caps)
        (only (hafod shell completers) command-flag-source register-completer!)
        (only (hafod posix) posix-getpwuid posix-getuid passwd-info-name)
        (chezscheme))

(test-begin "comp-dispatch")

;; ======================================================================
;; Helpers
;; ======================================================================

;; Drive read-expression over KEYS with capabilities forced on, so it emits its
;; full escape stream to a string port with no real terminal; end-of-input
;; submits, so the call returns at once.  Returns the captured bytes.
(define (drive-capture keys)
  (let ([sp (open-output-string)])
    (parameterize ([assume-terminal-caps 'on])
      (read-expression "> " (open-input-string keys) sp))
    (get-output-string sp)))

;; Like drive-capture, but also hands back read-expression's own result -- the
;; submitted line, which is the edit buffer at end-of-input -- so a test can
;; assert on the bytes INSERTED into the buffer as well as those emitted to the
;; screen.  Returns (values captured result).
(define (drive-capture+result keys)
  (let ([sp (open-output-string)])
    (parameterize ([assume-terminal-caps 'on])
      (let ([result (read-expression "> " (open-input-string keys) sp)])
        (values (get-output-string sp) result)))))

;; Naive substring search (no srfi-13 dependency in this suite).
(define (contains-substring? hay needle)
  (let ([hl (string-length hay)]
        [nl (string-length needle)])
    (let outer ([i 0])
      (cond
        [(> (+ i nl) hl) #f]
        [(string=? (substring hay i (+ i nl)) needle) #t]
        [else (outer (+ i 1))]))))

;; Fold the captured escape stream into the virtual terminal and report whether
;; any VISIBLE cell holds a byte a control-strip must have removed: a BEL (#x07),
;; a DEL (#x7f), or a C1 introducer (#x80..#x9f).  The fold is essential: the
;; editor's own screen setup legitimately emits an OSC sequence whose terminator
;; is a BEL, and a raw whole-stream scan would flag that terminator.  The vterm
;; consumes every CSI and OSC sequence (the OSC's BEL terminator among them), so a
;; BEL/DEL/C1 that survives as a glyph cell can only be a leaked candidate byte --
;; each such byte has display width one, so a leak does land as a cell.
(define (menu-has-spoof-glyph? captured cols)
  (let ([vt (make-vterm cols)])
    (vterm-feed! vt captured)
    (let rloop ([r 0])
      (cond
        [(>= r (vterm-rows vt)) #f]
        [(let cloop ([c 0])
           (cond
             [(>= c (vterm-cols vt)) #f]
             [(let ([cell (vterm-cell vt r c)])
                (and cell
                     (let ([code (char->integer (cell-glyph cell))])
                       (or (= code #x07) (= code #x7f)
                           (and (>= code #x80) (<= code #x9f))))))
              #t]
             [else (cloop (+ c 1))]))
         #t]
        [else (rloop (+ r 1))]))))

(define TAB (string #\tab))

;; ======================================================================
;; (a) --help option flags reach the menu.
;; `demo` has no registered completer, so a dash-prefixed argument token falls to
;; the option-flag completer.  Its producer is rebound to a fixed fixture so the
;; drive spawns nothing and is deterministic.  Before this dispatch existed a
;; dash token yielded filenames, so a pre-feature tree carries neither the flag
;; nor its description -- the assertions are non-vacuous.
;; ======================================================================

(let ([captured
       (parameterize ([command-flag-source
                       (lambda (cmd) '(("--foo" . "do foo") ("--bar" . "do bar")))])
         (drive-capture (string-append "demo --" TAB)))])
  (test-assert "dispatch: a --flag on a completer-less command offers its help flags"
    (contains-substring? captured "foo"))
  (test-assert "dispatch: the flag's help description reaches the rendered menu"
    (contains-substring? captured "do foo")))

;; ======================================================================
;; (b) A ~-token completes login names.
;; The current login is enumerated and typed back one character short, so the
;; full login only appears in the capture when the ~ arm intercepts and completes
;; it -- either as the sole match inserted into the line or among the menu
;; candidates.  A pre-feature tree lacks the arm and hands ~name to the filename
;; path, which prepends $HOME and never yields the login, so it fails this.
;; ======================================================================

(let* ([me (guard (e [#t #f]) (passwd-info-name (posix-getpwuid (posix-getuid))))])
  (test-assert "dispatch: the current login resolves for the ~user drive"
    (and (string? me) (> (string-length me) 0)))
  (when (and (string? me) (> (string-length me) 0))
    ;; One character short of the full login, so a bare echo of the typed prefix
    ;; cannot satisfy the assertion; only real completion produces the whole login.
    (let* ([typed (substring me 0 (max 0 (- (string-length me) 1)))]
           [captured (drive-capture (string-append "~" typed TAB))])
      (test-assert "dispatch: a ~-token completes the current login into the capture"
        (contains-substring? captured me)))))

;; ======================================================================
;; (c) The render strip sanitises an un-sanitising completer.
;; A throwaway command is registered to a completer whose two candidates embed a
;; raw BEL in their names and a raw DEL in their descriptions and that does NOT
;; strip its own output -- the same un-sanitised class as the shipped
;; git-branches/ssh-hosts completers.  Two candidates make the menu render rather
;; than auto-inserting a lone match.  The BEL sits AFTER the point the two names
;; diverge, so their longest common prefix -- the text actually inserted -- is
;; control-free, while each full name (shown only in the menu) still carries the
;; byte.  The rendered menu must therefore carry no BEL, DEL or C1 byte, proving
;; the strip happens at the single render choke-point regardless of the completer,
;; without disturbing the byte-exact insertion checked in (d).
;; ======================================================================

(register-completer! "spoofcmd"
  (lambda (prefix ctx)
    (list (list (string #\a #\a #\; #\b #\x07 #\b) '()
                (string #\d #\e #\s #\c #\x7f #\1))
          (list (string #\a #\a #\; #\c #\x07 #\c) '()
                (string #\d #\e #\s #\c #\x7f #\2)))))

(let ([captured (drive-capture (string-append "spoofcmd x" TAB))])
  (test-assert "dispatch: the menu did render for the control-bearing completer"
    (contains-substring? captured "aa;"))
  (test-assert "dispatch: the render strip removes raw control bytes from the menu"
    (not (menu-has-spoof-glyph? captured 80))))

;; ======================================================================
;; (d) Insertion is byte-exact; the strip is display-only.
;; A completer returns two candidates that DIVERGE after an embedded TAB, so
;; their longest common prefix already carries the raw byte.  A first TAB inserts
;; that prefix and opens the menu; a second cycles onto the first full candidate,
;; inserting it whole from completion-candidates.  read-expression returns the
;; submitted buffer, which must carry the candidate's ORIGINAL bytes -- a filename
;; with a control byte names the real file, not a stripped near-miss -- while the
;; rendered menu shows the same name with the byte removed.  The menu's stripped
;; fragment "QWen" cannot come from the byte-exact line (where a TAB sits between
;; Q and W), so it witnesses the display strip on its own; a whole-screen glyph
;; scan cannot be used here because the legitimate control byte now on the edit
;; line would trip it -- which is exactly the point.
;; ======================================================================

(register-completer! "byteexact"
  (lambda (prefix ctx)
    (list (list (string #\Q #\x09 #\W #\e #\n #\d) '() #f)
          (list (string #\Q #\x09 #\W #\x #\i #\t) '() #f))))

(let-values ([(captured result)
              (drive-capture+result (string-append "byteexact Q" TAB TAB))])
  (test-assert "dispatch: a control-bearing candidate inserts its ORIGINAL bytes"
    (and (string? result)
         (contains-substring? result (string #\Q #\x09 #\W #\e #\n #\d))))
  (test-assert "dispatch: the same candidate is shown in the menu with the byte stripped"
    (contains-substring? captured "QWen")))

(test-end)
