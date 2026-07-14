;;; test/test-history-exit-hook.ss -- Opening the history database must also
;;; arrange for it to be closed: running the exit hooks closes it.
;;;
;;; history-close! has always existed, and nothing ever called it.  The database
;;; was left to the kernel to reclaim at process teardown, which is not the same
;;; thing as closing it -- it hands SQLite no chance to finish what it was doing
;;; with the file.  open-history now registers history-close! on the exit hooks,
;;; and hafod's exit runs those hooks (call-exit-hooks-and-run), so every normal
;;; exit releases the database.
;;;
;;; What this suite observes is the database itself being CLOSED, not merely a
;;; hook having been registered -- a hook that ran and did nothing would satisfy
;;; the weaker claim.  So it watches the file: an entry added AFTER the hooks
;;; have run cannot reach it, because the handle it would have been written
;;; through is gone.
;;;
;;; Two controls keep that from being vacuous.  The first pins that entries added
;;; through this handle DO reach the file while it is open, so the instrument
;;; works.  The second pins that the later entry was still added -- it is held in
;;; memory -- so its absence from the file is the database being closed, and not
;;; an add that never happened.
;;;
;;; Exactly one history database is opened here, so exactly one close hook is
;;; under test.
;;;
;;; Entirely in-process -- one temporary file, no terminal -- so it runs the same
;;; way everywhere.  Degrades to a printed note where libsqlite3 cannot be loaded
;;; at all.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor history)
        (only (hafod exit-hooks) call-exit-hooks!)
        (only (hafod editor sqlite3)
              sqlite3-open sqlite3-close sqlite3-prepare sqlite3-step
              sqlite3-finalize sqlite3-column-text sqlite3-loaded?
              SQLITE_ROW)
        (only (hafod posix) posix-mkstemp posix-close posix-unlink)
        (chezscheme))

;; A fresh temporary database path.  mkstemp leaves a zero-length file behind,
;; which SQLite opens as an empty database.
(define (temp-db-path label)
  (let-values ([(path fd) (posix-mkstemp
                            (string-append "/tmp/hafod-history-" label "-XXXXXX"))])
    (posix-close fd)
    path))

;; Remove a temporary database and any sidecar SQLite may have written beside
;; it, so the suite leaves nothing behind.  Each unlink is guarded: a sidecar
;; that was never created is the normal case, not an error.
(define (remove-db! path)
  (for-each
    (lambda (suffix)
      (guard (e [#t #f]) (posix-unlink (string-append path suffix))))
    '("" "-journal" "-wal" "-shm")))

;; The inputs on the file, oldest first, read on a connection of its own -- so
;; what is asserted is what reached the FILE, never what the history handle
;; happens to be holding in memory.
(define (file-inputs path)
  (let* ([db (sqlite3-open path)]
         [stmt (and db (sqlite3-prepare db
                         "SELECT input FROM history ORDER BY id"))])
    (let loop ([acc '()])
      (if (and stmt (= (sqlite3-step stmt) SQLITE_ROW))
          (loop (cons (sqlite3-column-text stmt 0) acc))
          (begin
            (when stmt (sqlite3-finalize stmt))
            (when db (sqlite3-close db))
            (reverse acc))))))

(define (note . parts)
  (display "  note: ")
  (for-each display parts)
  (newline))

(test-begin "history-exit-hook")

(define db-path (temp-db-path "exit-hook"))

;; The one and only history database this suite opens -- and the one close hook
;; it therefore puts on the exit hooks.  open-history goes through the sqlite3
;; wrappers, whose shared object is resolved on first use, so this call is what
;; loads it and sqlite3-loaded? can be trusted from here on.
(define h (open-history db-path))

(if (not (sqlite3-loaded?))
    (begin
      (note "libsqlite3 is unavailable; skipping the exit-hook assertions")
      (history-close! h)
      (remove-db! db-path))
    (let ([before "(display \"submitted while the database was open\")"]
          [after  "(display \"submitted after the exit hooks ran\")"])

      (history-add! h before)

      ;; Control: entries added through this handle reach the file.  Without
      ;; this, an absent entry below would prove nothing -- a handle that never
      ;; wrote anything would look exactly the same.
      (test-equal "an entry added while the database is open reaches the file"
        (list before)
        (file-inputs db-path))

      ;; The exit path hafod really takes: (hafod process)'s exit runs the hooks
      ;; before it flushes and leaves.  If open-history registered the close,
      ;; this shuts the database.
      (call-exit-hooks!)

      (history-add! h after)

      ;; Control: the later entry WAS added -- the handle took it into memory.
      ;; So whether it reached the file is a question about the database being
      ;; open, and not about whether the add ran at all.
      (test-equal "the later entry is still taken in memory, so the add did happen"
        2
        (history-count h))

      ;; The observation.  The database was closed by the hook, so the entry the
      ;; handle accepted had nowhere to go: it is not on the file.  Pre-fix,
      ;; nothing registered a close, the database stayed open, and this entry
      ;; persisted alongside the first.
      (test-equal
        "running the exit hooks closes the database, so a later entry cannot reach the file"
        (list before)
        (file-inputs db-path))

      (remove-db! db-path)))

(test-end)
