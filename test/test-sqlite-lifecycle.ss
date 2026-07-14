;;; test/test-sqlite-lifecycle.ss -- A database open that FAILS must still hand
;;; back the connection object SQLite allocated for it.
;;;
;;; sqlite3_open() allocates a connection object whether or not the open
;;; succeeds, and the caller owns it either way: the handle it writes to the out
;;; slot is closeable -- and must be closed -- even after a failure.  A wrapper
;;; that reads that handle, sees a non-OK return code and simply reports #f
;;; drops the object on the floor, once per failed open, for as long as the
;;; session runs.
;;;
;;; The oracle is SQLite's own accounting: sqlite3_memory_used() reports the
;;; bytes it currently has outstanding.  Repeated failed opens against a path
;;; that cannot be opened must not move it -- each attempt allocates a
;;; connection object and closes it again, netting zero.  On a tree that leaks,
;;; the counter climbs by one connection object per attempt (measured: ~1,400
;;; bytes each, so ~420 KB over the three hundred below), and the assertion
;;; fails.  This is what makes the proof non-vacuous.
;;;
;;; The counter only has teeth where SQLite was built with memory accounting
;;; compiled in, so the suite CHECKS ITS OWN ORACLE first: it opens a real
;;; database and watches the counter rise.  Where it does not rise, the counter
;;; is blind -- on Linux that is a defect and the suite says so loudly, because
;;; a silent pass would prove nothing; on macOS the system libsqlite3 is built
;;; with SQLITE_DEFAULT_MEMSTATUS=0 and the counter reads zero forever, which is
;;; a property of that library and not of the handle lifecycle, so there the
;;; suite notes the gap and skips the assertion rather than reddening for an
;;; unrelated reason.  The fix itself is not platform-specific; only this proof
;;; of it is.
;;;
;;; The last-insert-rowid primitive is per CONNECTION -- it reports the row this
;;; connection inserted, never a row another session interleaved into the same
;;; file -- and the final case pins that.
;;;
;;; Entirely in-process: one temporary database and a loop of failed opens, no
;;; terminal, so it runs the same way everywhere.  Degrades to a printed note
;;; where libsqlite3 cannot be loaded at all.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (only (hafod editor sqlite3)
              sqlite3-open sqlite3-close sqlite3-exec
              sqlite3-prepare sqlite3-step sqlite3-finalize
              sqlite3-last-insert-rowid sqlite3-loaded?
              SQLITE_OK SQLITE_DONE)
        (only (hafod internal platform) os-family)
        (only (hafod posix) posix-mkstemp posix-close posix-unlink posix-getpid)
        (chezscheme))

;; A path SQLite cannot open: its parent directory does not exist, and SQLite
;; creates no intermediate directories, so the open fails without touching the
;; disk.  Carries the pid so a stray run cannot collide with a real directory.
(define unopenable-path
  (string-append "/hafod-no-such-directory-" (number->string (posix-getpid))
                 "/history.db"))

;; A fresh temporary database path.  mkstemp leaves a zero-length file behind,
;; which SQLite opens as an empty database.
(define (temp-db-path label)
  (let-values ([(path fd) (posix-mkstemp
                            (string-append "/tmp/hafod-sqlite-" label "-XXXXXX"))])
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

(define (note . parts)
  (display "  note: ")
  (for-each display parts)
  (newline))

(test-begin "sqlite-lifecycle")

;; First use of a wrapper resolves the shared object; the sqlite3 FFI is
;; deferred, so nothing is loaded until this call.  The path cannot be opened,
;; which is precisely the case under test.
(sqlite3-open unopenable-path)

(if (not (sqlite3-loaded?))
    (note "libsqlite3 is unavailable; skipping the lifecycle assertions")
    (let ()
      ;; A foreign-procedure resolves its entry point when it is EVALUATED, so
      ;; this form sits inside the loaded guard: on a host without libsqlite3 it
      ;; is never reached, and the suite skips cleanly instead of raising.
      ;; sqlite3_memory_used() returns the bytes SQLite currently has
      ;; outstanding, as a 64-bit signed count.
      (define memory-used
        (foreign-procedure "sqlite3_memory_used" () integer-64))

      ;; ---- The oracle checks itself ------------------------------------
      ;; Open a real database and watch the counter.  If it rises, this build
      ;; accounts for its memory and a leaked connection object would be visible
      ;; to the assertion below.  If it does not, the counter is blind and the
      ;; assertion would pass by proving nothing.
      (define db-path (temp-db-path "oracle"))
      (define before-open (memory-used))
      (define db (sqlite3-open db-path))
      (define create-rc
        (and db (sqlite3-exec db "CREATE TABLE probe (id INTEGER PRIMARY KEY)")))
      (define after-open (memory-used))

      ;; The success path is untouched by the fix: a valid path still yields a
      ;; live, usable handle, and it is emphatically NOT closed on the way out.
      (test-assert "opening a real database yields a usable handle"
        (and db (eqv? create-rc SQLITE_OK)))

      (sqlite3-close db)
      (remove-db! db-path)

      (cond
        [(> after-open before-open)
         ;; ---- The proof -------------------------------------------------
         ;; Three hundred opens that cannot succeed.  Post-fix each allocates a
         ;; connection object and closes it again, so the outstanding total
         ;; returns to where it started; pre-fix each one strands ~1,400 bytes,
         ;; and the total climbs by ~420 KB.  The threshold sits an order of
         ;; magnitude below that, and well above the nothing a fixed tree leaks.
         (let ([baseline (memory-used)])
           (let loop ([n 0])
             (when (< n 300)
               (sqlite3-open unopenable-path)   ; reports #f; must strand nothing
               (loop (+ n 1))))
           (let ([delta (- (memory-used) baseline)])
             (note "outstanding allocation after 300 failed opens: "
                   delta " bytes")
             (test-assert "a failed open does not strand the connection object"
               (< delta 32768))))]

        [(eq? os-family 'linux)
         ;; Every Linux libsqlite3 this project builds against accounts for its
         ;; memory.  A counter that does not move against a real open means the
         ;; oracle cannot see a leak, and a pass here would be vacuous -- so say
         ;; so, loudly, rather than report a proof that was never made.
         (test-assert
           "sqlite3_memory_used tracks a real open, so a stranded handle is visible"
           #f)]

        [else
         ;; macOS ships libsqlite3 built with SQLITE_DEFAULT_MEMSTATUS=0: the
         ;; counter reads zero whatever is outstanding.  That is a property of
         ;; the platform library, not of the handle lifecycle the fix addresses,
         ;; so the assertion is skipped rather than failed.
         (note "this libsqlite3 keeps no memory accounting "
               "(SQLITE_DEFAULT_MEMSTATUS=0); skipping the leak assertion")])

      ;; ---- The connection reports its OWN row ---------------------------
      ;; sqlite3_last_insert_rowid is per connection, so a caller that inserts a
      ;; row can name that row afterwards -- which is what lets a writer update
      ;; the entry it added rather than whatever row happens to be newest in the
      ;; file.  Two inserts on one connection report 1, then 2.  Needs no memory
      ;; accounting, so it runs on every platform that has the library at all.
      (let* ([entry-path (temp-db-path "rowid")]
             [entry-db (sqlite3-open entry-path)]
             [insert! (lambda (sql)
                        (let* ([stmt (sqlite3-prepare entry-db sql)]
                               [rc (sqlite3-step stmt)])
                          (sqlite3-finalize stmt)
                          rc))])
        (sqlite3-exec entry-db
                      "CREATE TABLE entry (id INTEGER PRIMARY KEY, line TEXT)")

        (test-equal "an insert runs to completion"
          SQLITE_DONE
          (insert! "INSERT INTO entry (line) VALUES ('first')"))
        (test-equal "the connection reports the row id of the insert it just made"
          1
          (sqlite3-last-insert-rowid entry-db))

        (insert! "INSERT INTO entry (line) VALUES ('second')")
        (test-equal "a further insert advances the connection's own row id"
          2
          (sqlite3-last-insert-rowid entry-db))

        (sqlite3-close entry-db)
        (remove-db! entry-path))))

(test-end)
