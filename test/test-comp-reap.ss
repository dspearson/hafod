;;; test-comp-reap.ss -- Completion subprocess reap and terminal-escape strip.
;;; The strip cases prove a colour-wrapped, OSC-8-wrapped or control-laden
;;; candidate reduces to its visible text; the Linux /proc block proves that a
;;; burst of completer spawns leaves no defunct children behind.
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (chezscheme)
        (test runner)
        (hafod shell completers)
        (only (hafod posix) posix-waitpid))

(test-begin "comp-reap")

;; === strip-terminal-escapes ===
;; Each ESC below is written "\x1b;" -- the trailing semicolon terminates the
;; hex escape, so the byte is a single ESC (#x1b), not ESC followed by ";".

;; A CSI colour run around a word reduces to the bare word.
(test-equal "strip: CSI colour wrapper removed"
  "red"
  (strip-terminal-escapes "\x1b;[31mred\x1b;[0m"))

;; An OSC-8 hyperlink (ESC ] 8 ; ; URL ESC \ ... ESC ] 8 ; ; ESC \) around
;; visible text reduces to just the text.
(test-equal "strip: OSC-8 hyperlink wrapper removed"
  "link"
  (strip-terminal-escapes
    (string-append "\x1b;]8;;https://example.com\x1b;\\"
                   "link"
                   "\x1b;]8;;\x1b;\\")))

;; Bare carriage return, line feed, tab, backspace, DEL and a C1 introducer
;; (#x9b) are dropped; the surrounding printable text is untouched.
(test-equal "strip: bare C0/C1/DEL bytes removed"
  "abcXYZ"
  (strip-terminal-escapes "a\x0d;b\x0a;c\x09;\x08;\x7f;\x9b;XYZ"))

;; Ordinary printable text passes through unchanged.
(test-equal "strip: plain text unchanged"
  "plain text 123"
  (strip-terminal-escapes "plain text 123"))

;; A sequence truncated at end-of-string stops safely, keeping the prefix.
(test-equal "strip: truncated CSI at end stops safely"
  "ok"
  (strip-terminal-escapes "ok\x1b;[31"))

;; === Subprocess reap (Linux-gated; self-skips where /proc is absent) ===

;; A burst of completer spawns must leave no unreaped child behind.  Each
;; kill-completer call spawns `ps` through command-output, which reaps its child
;; before returning, so an immediate drain of finished children should find
;; nothing left.  The count is taken with a non-blocking waitpid rather than by
;; walking /proc for defunct entries on purpose: a /proc walk allocates heavily
;; and would trigger the very collection whose guardian reaps these children
;; anyway, masking a genuine leak and leaving the proof toothless.  A waitpid
;; poll allocates almost nothing, so a tree without the synchronous reap still
;; shows its lingering children here and fails the assertion.  Gated on /proc so
;; it runs on Linux and self-skips on macOS, whose process model this narrow
;; proof does not target.
(when (file-exists? "/proc")
  (let ()
    ;; Reap and count every already-finished child of this process.  A
    ;; non-blocking waitpid (the 1 is WNOHANG) returns a positive pid for each
    ;; finished child in turn, 0 while a child is still running, and raises when
    ;; no children remain at all -- swallowed here as "none left".
    (define (drain-finished-children)
      (let loop ([n 0])
        (let ([r (guard (e [#t 'none])
                   (let-values ([(w s) (posix-waitpid -1 1)]) w))])
          (cond
            [(eq? r 'none) n]
            [(and (integer? r) (> r 0)) (loop (+ n 1))]
            [else n]))))
    (drain-finished-children)    ; clear any child left by earlier scaffolding
    (do ([i 0 (+ i 1)]) ((= i 40)) (kill-completer "1" '((args))))
    (test-assert "reap: a burst of completer spawns leaves no unreaped child"
      (= (drain-finished-children) 0))))

(test-end)
