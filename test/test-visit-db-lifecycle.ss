;;; test/test-visit-db-lifecycle.ss -- The directory visit database is hardened
;;; exactly like the history database: owner-only, closed on exit, and never
;;; able to take the shell down with it.
;;;
;;; The visit database holds the names of the directories the user works in --
;;; their projects, their clients, the paths that give away what they are doing.
;;; It is held to the same rule as the history file: 0600, and the -wal/-shm
;;; sidecars 0600 with it.  SQLite gives a sidecar it creates the mode of the
;;; database file at that moment, so tightening the database before the first
;;; write tightens the sidecars too -- a claim about a library this suite does
;;; not own, so it is checked here by forcing the -wal into existence and reading
;;; its mode off the filesystem.
;;;
;;; The umask is forced to 0022 first.  Without that the suite would prove
;;; nothing on a machine whose umask is already 0077 -- SQLite would create the
;;; file 0600 unaided and the assertions would pass on any tree at all.  Restored
;;; at the end.
;;;
;;; Two more things it observes: that opening the database registers a close on
;;; the exit hooks (a visit recorded after the hooks have run cannot reach the
;;; file, because the connection it would have gone through is closed), and that
;;; every entry point over a database that could not open degrades without
;;; raising -- the latter needs no libsqlite3, so it runs unconditionally and
;;; keeps the suite non-vacuous even where the library is absent.
;;;
;;; Entirely in-process -- temporary files, no terminal -- so it runs the same
;;; way on both platforms.  Degrades to a printed note where libsqlite3 cannot be
;;; loaded.
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod shell visit-db)
        (only (hafod editor sqlite3)
              sqlite3-loaded? sqlite3-open sqlite3-close sqlite3-prepare
              sqlite3-step sqlite3-finalize sqlite3-column-text SQLITE_ROW)
        (only (hafod exit-hooks) call-exit-hooks!)
        (only (hafod posix)
              posix-mkstemp posix-close posix-unlink posix-chmod posix-umask
              posix-stat stat-info-mode)
        (chezscheme))

;; A uniquely reserved name.  mkstemp creates the file and hands back a name no
;; other mkstemp will give out while it exists, so the file is KEPT -- it is the
;; reservation -- and the database is opened at a name derived from it, which
;; does not exist, so SQLite creates it itself with the mode under test.
(define (reserve-name label)
  (let-values ([(path fd) (posix-mkstemp
                            (string-append "/tmp/hafod-visits-" label "-XXXXXX"))])
    (posix-close fd)
    path))

;; A fresh temporary database path (the mkstemp file itself, opened directly as
;; an empty database).
(define (temp-db-path label)
  (let-values ([(path fd) (posix-mkstemp
                            (string-append "/tmp/hafod-visits-" label "-XXXXXX"))])
    (posix-close fd)
    path))

;; The permission bits of a file, or #f when it is not there.  Masked to 0777:
;; the file-type bits above them are not what is being asserted.
(define (mode-of path)
  (guard (e [#t #f])
    (bitwise-and (stat-info-mode (posix-stat path)) #o777)))

;; Remove a reservation, the database opened beside it, and any sidecar SQLite
;; left there.  Each unlink is guarded: a file that was never created is normal.
(define (clean-up! base)
  (for-each
    (lambda (suffix)
      (guard (e [#t #f]) (posix-unlink (string-append base suffix))))
    '("" ".db" ".db-journal" ".db-wal" ".db-shm")))

;; Remove a temporary database opened directly at its own path, and its sidecars.
(define (remove-db! path)
  (for-each
    (lambda (suffix)
      (guard (e [#t #f]) (posix-unlink (string-append path suffix))))
    '("" "-journal" "-wal" "-shm")))

;; The paths on the file, oldest first, read on a connection of its own -- so
;; what is asserted is what reached the FILE, never what a handle happens to be
;; holding in memory.
(define (file-paths path)
  (let* ([db (sqlite3-open path)]
         [stmt (and db (sqlite3-prepare db
                         "SELECT path FROM visits ORDER BY last_access"))])
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

(test-begin "visit-db-lifecycle")

;; ----------------------------------------------------------------------
;; Fail-quiet.  Needs no libsqlite3 (a database that will not open leaves the
;; handle holding a #f connection either way), so it runs unconditionally and
;; keeps the suite non-vacuous on a host without the library.
;; ----------------------------------------------------------------------

;; A directory that cannot be created -- the parent does not exist -- so the open
;; fails and every entry point must return quietly rather than raise.
(define dead-path "/nonexistent-dir-zz/visits.db")

(test-assert "recording to a database that could not open does not raise"
  (begin (visit-record! (open-visit-db dead-path) "/somewhere" 0)
         #t))

(test-assert "querying a handle with no live database returns #f, not a raise"
  (eq? #f (visit-query-best (open-visit-db dead-path) "anything" 0)))

(test-assert "listing candidates over a handle with no live database returns '()"
  (null? (visit-candidates (open-visit-db dead-path) 0)))

;; ----------------------------------------------------------------------
;; The hardened lifecycle.  Needs libsqlite3.
;; ----------------------------------------------------------------------

;; A permissive umask, the one a login shell hands its children.  Left in place
;; for the whole suite and restored at the end; every process here is this one.
(define original-umask (posix-umask #o022))

;; open-visit-db goes through the sqlite3 wrappers, whose shared object is
;; resolved on first use -- so this first open is what loads it, and
;; sqlite3-loaded? can be trusted from here on.
(define fresh-base (reserve-name "fresh"))
(define fresh-db (string-append fresh-base ".db"))
(define h (open-visit-db fresh-db))

(if (not (sqlite3-loaded?))
    (begin
      (note "libsqlite3 is unavailable; skipping the file-permission and exit-hook assertions")
      (visit-db-close! h)
      (clean-up! fresh-base)
      (posix-umask original-umask))
    (begin

      ;; Control: the file really was created by this open, and is there to be
      ;; read.  A mode-of of #f below would otherwise read as "not 0600" and pass.
      (test-assert "opening a visit database creates the file"
        (mode-of fresh-db))

      ;; The observation.  0600, not the 0644 SQLite makes under this umask.
      (test-equal "a new visit database is readable only by its owner"
        #o600
        (mode-of fresh-db))

      ;; Write a visit, which is what puts a -wal on the disk: the sidecars appear
      ;; at the first write, not at the PRAGMA that asks for WAL.
      (visit-record! h "/home/user/some-private-project" 1700000000)

      ;; Control: there IS a -wal to inspect.
      (test-assert "recording a visit puts a -wal beside the database"
        (mode-of (string-append fresh-db "-wal")))

      ;; The claim the design rests on, checked rather than taken on trust: SQLite
      ;; gives the sidecar the database's mode, so tightening the database at open
      ;; tightens the -wal that appears later -- the file the visits actually sit
      ;; in until a checkpoint moves them.
      (test-equal "the write-ahead log beside it is readable only by its owner"
        #o600
        (mode-of (string-append fresh-db "-wal")))

      ;; The shared-memory index is created alongside the -wal on a filesystem
      ;; that takes one.  Where it exists it is held to the same rule; where it
      ;; does not, there is nothing to assert and this passes.
      (test-assert "the shared-memory index, where there is one, is readable only by its owner"
        (let ([shm (mode-of (string-append fresh-db "-shm"))])
          (or (not shm) (= shm #o600))))

      (visit-db-close! h)
      (clean-up! fresh-base)

      ;; ------------------------------------------------------------------
      ;; ":memory:" -- SQLite's in-memory database.  There is no file, so
      ;; there is nothing to chmod; the open must go through untouched.
      ;; ------------------------------------------------------------------

      (let ([mem (open-visit-db ":memory:")])
        (test-assert "an in-memory visit database opens and works, having no file to tighten"
          (and (visitdb? mem)
               (begin (visit-record! mem "/in/memory/nowhere/on/disk" 1700000000)
                      #t)))
        (visit-db-close! mem))

      ;; And it left no file behind called ":memory:" -- a chmod of that name
      ;; would have failed, but a CREATE of it would be worse.
      (test-assert "an in-memory visit database writes no file called \":memory:\""
        (not (mode-of ":memory:")))

      ;; ------------------------------------------------------------------
      ;; Close-on-exit.  Opening the database registers a close on the exit
      ;; hooks; running them shuts it, so a visit recorded afterwards has
      ;; nowhere to go.
      ;; ------------------------------------------------------------------

      (let* ([hook-db (temp-db-path "exit-hook")]
             [vh (open-visit-db hook-db)])

        (visit-record! vh "/first/visit" 1700000000)

        ;; Control: a visit recorded through this handle reaches the file.  Without
        ;; this, an absent visit below would prove nothing -- a handle that never
        ;; wrote anything would look exactly the same.
        (test-equal "a visit recorded while the database is open reaches the file"
          (list "/first/visit")
          (file-paths hook-db))

        ;; The exit path hafod really takes: its exit runs the hooks before it
        ;; flushes and leaves.  If open-visit-db registered the close, this shuts
        ;; the database.
        (call-exit-hooks!)

        (visit-record! vh "/second/visit" 1700000100)

        ;; The observation.  The database was closed by the hook, so the visit the
        ;; handle accepted had nowhere to go: it is not on the file.
        (test-equal "after the exit hooks run, a later visit cannot reach the file"
          (list "/first/visit")
          (file-paths hook-db))

        (remove-db! hook-db))

      (posix-umask original-umask)))

(test-end)
