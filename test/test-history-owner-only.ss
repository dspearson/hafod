;;; test/test-history-owner-only.ss -- The history file is readable only by its owner.
;;;
;;; The history database holds every line the user typed, verbatim: the exported
;;; API key, the token pasted into a curl header, the password that went on a
;;; command line by mistake.  It is exactly the file bash, zsh and fish all keep at
;;; 0600, and hafod's was left at whatever SQLite makes -- 0666 & ~umask, so 0644
;;; on the umask a login shell actually has.  World-readable.
;;;
;;; The sidecars matter as much as the database.  A WAL database keeps a -wal
;;; beside it holding the commands most recently written -- they sit there,
;;; unmerged, until a checkpoint -- and SQLite gives a -wal/-shm it creates the mode
;;; of the DATABASE FILE at the moment it creates them.  So tightening the database
;;; before the first write tightens the sidecars with it.  That is the whole reason
;;; the mode is set at open, and it is a claim about a library this suite does not
;;; own, so it is checked here rather than assumed: the -wal is forced into
;;; existence and its mode is read off the filesystem.
;;;
;;; Three files, three ways in:
;;;   * a database SQLite creates from nothing, which is the fresh install;
;;;   * a database already on disk at 0644, which is every existing user, and which
;;;     must be repaired on the next open without losing what is in it;
;;;   * ":memory:", which is not a file at all and must not be chmod'ed or fail.
;;;
;;; The umask is forced to 0022 first.  Without that the suite would prove nothing
;;; on a machine whose umask is already 0077 -- SQLite would create the file 0600
;;; unaided and the assertions would pass on any tree at all.  Restored at the end.
;;;
;;; Entirely in-process -- temporary files, no terminal -- so it runs the same way
;;; on both platforms.  Degrades to a printed note where libsqlite3 cannot be loaded.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor history)
        (only (hafod editor sqlite3) sqlite3-loaded?)
        (only (hafod posix)
              posix-mkstemp posix-close posix-unlink posix-chmod posix-umask
              posix-stat stat-info-mode)
        (chezscheme))

;; A uniquely reserved name.  mkstemp creates the file and hands back a name no
;; other mkstemp will give out while it exists, so the file is KEPT -- it is the
;; reservation -- and the database is opened at a name derived from it.  That name
;; does not exist, so SQLite creates it itself, with the mode it chooses, which is
;; the thing under test.  (Unlinking the reservation and reusing its name would
;; hand it back to the next mkstemp on the machine, so it is left alone.)
(define (reserve-name label)
  (let-values ([(path fd) (posix-mkstemp
                            (string-append "/tmp/hafod-history-" label "-XXXXXX"))])
    (posix-close fd)
    path))

;; The permission bits of a file, or #f when it is not there.  Masked to 0777: the
;; file-type bits above them are not what is being asserted.
(define (mode-of path)
  (guard (e [#t #f])
    (bitwise-and (stat-info-mode (posix-stat path)) #o777)))

;; Remove a reservation, the database opened beside it, and any sidecar SQLite left
;; there.  Each unlink is guarded: a file that was never created is the normal case.
(define (clean-up! base)
  (for-each
    (lambda (suffix)
      (guard (e [#t #f]) (posix-unlink (string-append base suffix))))
    '("" ".db" ".db-journal" ".db-wal" ".db-shm")))

(define (note . parts)
  (display "  note: ")
  (for-each display parts)
  (newline))

(test-begin "history-owner-only")

;; A permissive umask, the one a login shell hands its children.  Left in place for
;; the whole suite and restored at the end; every process here is this one.
(define original-umask (posix-umask #o022))

;; open-history goes through the sqlite3 wrappers, whose shared object is resolved
;; on first use -- so the first open below is what loads it, and sqlite3-loaded? can
;; be trusted from there on.
(define fresh-base (reserve-name "owner-only-fresh"))
(define fresh-db (string-append fresh-base ".db"))
(define h (open-history fresh-db))

(if (not (sqlite3-loaded?))
    (begin
      (note "libsqlite3 is unavailable; skipping the file-permission assertions")
      (history-close! h)
      (clean-up! fresh-base)
      (posix-umask original-umask))
    (begin

      ;; ------------------------------------------------------------------
      ;; A database SQLite created from nothing -- the fresh install.
      ;; ------------------------------------------------------------------

      ;; Control: the file really was created by this open, and is there to be read.
      ;; A mode-of of #f below would otherwise read as "not 0644" and pass.
      (test-assert "opening a history database creates the file"
        (mode-of fresh-db))

      ;; The observation.  0600, not the 0644 SQLite makes under this umask.
      (test-equal "a new history database is readable only by its owner"
        #o600
        (mode-of fresh-db))

      ;; Write through the history itself, which is what puts a -wal on the disk:
      ;; the sidecars appear at the first write, not at the PRAGMA that asks for WAL.
      (history-add! h "export API_KEY=sk-not-for-your-neighbours")

      ;; Control: there IS a -wal to inspect.  Without this the mode assertion below
      ;; would be asserting the permissions of a file that does not exist.
      (test-assert "writing to the history puts a -wal beside the database"
        (mode-of (string-append fresh-db "-wal")))

      ;; The claim the fix rests on, checked rather than taken on trust: SQLite gives
      ;; the sidecar the database's mode, so tightening the database at open tightens
      ;; the -wal that appears later -- the file the commands are actually sitting in
      ;; until a checkpoint moves them.
      (test-equal "the write-ahead log beside it is readable only by its owner"
        #o600
        (mode-of (string-append fresh-db "-wal")))

      ;; The shared-memory index is created alongside the -wal on a filesystem that
      ;; takes one.  Where it exists it is held to the same rule; where it does not,
      ;; there is nothing to assert and this passes.
      (test-assert "the shared-memory index, where there is one, is readable only by its owner"
        (let ([shm (mode-of (string-append fresh-db "-shm"))])
          (or (not shm) (= shm #o600))))

      (history-close! h)
      (clean-up! fresh-base)

      ;; ------------------------------------------------------------------
      ;; A database already on disk at 0644 -- every user who has run hafod
      ;; before.  Opening it must repair the mode, and must not cost them
      ;; their history to do it.
      ;; ------------------------------------------------------------------

      (let* ([legacy-base (reserve-name "owner-only-legacy")]
             [legacy-db (string-append legacy-base ".db")]
             [old-command "psql postgres://user:hunter2@db.internal/prod"])

        ;; A history file as it was written before any of this: a real database with
        ;; a real line in it, left world-readable.
        (let ([old (open-history legacy-db)])
          (history-add! old old-command)
          (history-close! old))
        (posix-chmod legacy-db #o644)

        ;; Control: it really is world-readable going in.  A file that was somehow
        ;; already 0600 would make the repair below unobservable.
        (test-equal "an existing history database can be found world-readable"
          #o644
          (mode-of legacy-db))

        (let ([reopened (open-history legacy-db)])
          ;; The observation.  Opening an old history file repairs it.
          (test-equal "an existing world-readable history database is tightened when it is opened"
            #o600
            (mode-of legacy-db))

          ;; And the repair is a repair, not a reset: the line that was in there is
          ;; still in there, and still readable by the session that owns it.
          (test-equal "tightening an existing history database does not lose what is in it"
            (list 1 old-command)
            (list (history-count reopened) (history-ref reopened 0)))

          (history-close! reopened))

        (clean-up! legacy-base))

      ;; ------------------------------------------------------------------
      ;; ":memory:" -- SQLite's in-memory database.  There is no file, so
      ;; there is nothing to chmod; the open must go through untouched.
      ;; ------------------------------------------------------------------

      (let ([mem (open-history ":memory:")])
        (test-assert "an in-memory history opens and works, having no file to tighten"
          (and (history? mem)
               (begin (history-add! mem "in memory, and nowhere on the disk")
                      (= (history-count mem) 1))))
        (history-close! mem))

      ;; And it left no file behind called ":memory:" -- a chmod of that name would
      ;; have failed, but a CREATE of it would be worse.
      (test-assert "an in-memory history writes no file called \":memory:\""
        (not (mode-of ":memory:")))

      (posix-umask original-umask)))

(test-end)
