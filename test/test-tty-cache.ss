;;; test/test-tty-cache.ss -- The SIGWINCH-refreshed terminal-size cache.
;;; Proves the leaf width cache is genuinely cache-backed: seeding it with a
;;; sentinel size distinct from both the 24x80 non-terminal fallback and any
;;; plausible real terminal size, then reading it back, returns the SEEDED
;;; value -- so a reader consults the cached cell, not a fresh TIOCGWINSZ ioctl
;;; (which off this non-terminal test process would report 24x80).  Entirely
;;; PTY-free: it opens no terminal and needs no platform gate, so it runs
;;; identically on every platform.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod tty)
              cached-terminal-size
              refresh-terminal-size-cache!
              set-terminal-size-cache!))

(test-begin "tty-cache")

;; Seed a sentinel no live ioctl off a non-terminal could return: the fallback
;; is 24x80, so reading back 40x200 can only have come from the cached cell.
(set-terminal-size-cache! 40 200)
(let-values ([(rows cols) (cached-terminal-size)])
  (test-equal "cached rows read from the seeded cell" 40 rows)
  (test-equal "cached cols read from the seeded cell" 200 cols))

;; A second, different sentinel: the reader tracks whatever was last stored,
;; confirming it reads the live cell rather than returning a one-shot constant.
(set-terminal-size-cache! 51 137)
(let-values ([(rows cols) (cached-terminal-size)])
  (test-equal "cached rows follow a re-seed" 51 rows)
  (test-equal "cached cols follow a re-seed" 137 cols))

;; refresh-terminal-size-cache! runs the live query without raising off a
;; non-terminal and leaves a readable, positive (rows . cols) behind.  Off this
;; non-terminal process the live query yields the 24x80 fallback; on a real
;; terminal it yields the true size -- either way both counts stay positive, so
;; the smoke check is environment-independent.
(refresh-terminal-size-cache!)
(let-values ([(rows cols) (cached-terminal-size)])
  (test-assert "refreshed rows are a positive count" (and (integer? rows) (> rows 0)))
  (test-assert "refreshed cols are a positive count" (and (integer? cols) (> cols 0))))

(test-end)
