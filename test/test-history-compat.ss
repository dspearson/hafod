;;; test/test-history-compat.ss -- A history file written before the database was
;;; ever configured for concurrency still opens, still reads back exactly as it
;;; was written, and is carried over to WAL on the way.
;;;
;;; Opening the database in WAL changes the file.  The journal mode lives in the
;;; database header and the change is durable, so the first open of a history file
;;; a user already has rewrites that header -- and everything they have ever typed
;;; is in that file.  A concurrency fix that ate somebody's history would be a far
;;; worse trade than the lost write it was meant to save.
;;;
;;; So this suite builds the database the old code left behind -- the schema it
;;; created, in the order it created it, and not one journal_mode PRAGMA anywhere
;;; near it, so the file keeps SQLite's default rollback journal exactly as a real
;;; history.db does -- fills it with known lines in known modes, and then opens it
;;; the way hafod now does.
;;;
;;; Two things must both hold.  Every row must read back unchanged: each input,
;;; and the mode it was written with.  And the upgrade must actually have happened
;;; -- read the journal mode back off the file on a connection of its own, WHILE the
;;; session that upgraded it is still open, and it says "wal" where it used to say
;;; "delete".  Without the second, the first is only a test that reading works;
;;; without the first, the upgrade could be eating rows and nobody here would know.
;;; The premise is pinned too: the file really does start as a rollback-journal
;;; database, so the upgrade has something to do.
;;;
;;; The mode is asked of a LIVE session because that is the only time it governs
;;; anything.  WAL is here to let a second session read while this one writes, and
;;; that connection of its own is precisely the second reader it exists to permit.
;;; What the session leaves behind is a different question, asked separately at the
;;; end: closing checkpoints the log back into the database and takes the journal
;;; with it, so a settled history is ONE self-contained file -- not a database with
;;; a -wal beside it still holding the lines the user typed.
;;;
;;; Entirely in-process -- one temporary file, no terminal -- so it runs the same
;;; way everywhere.  Degrades to a printed note where libsqlite3 cannot be loaded.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor history)
        (only (hafod editor sqlite3)
              sqlite3-open sqlite3-close sqlite3-exec sqlite3-prepare
              sqlite3-step sqlite3-finalize sqlite3-bind-text
              sqlite3-column-text sqlite3-loaded?
              SQLITE_OK SQLITE_ROW SQLITE_DONE)
        (only (hafod posix) posix-mkstemp posix-close posix-unlink)
        (chezscheme))

;; A fresh temporary database path.  mkstemp leaves a zero-length file behind,
;; which SQLite opens as an empty database.
(define (temp-db-path label)
  (let-values ([(path fd) (posix-mkstemp
                            (string-append "/tmp/hafod-history-" label "-XXXXXX"))])
    (posix-close fd)
    path))

;; Remove a temporary database and any sidecar SQLite may have written beside it
;; -- a WAL database keeps a -wal and a -shm next to it while a connection is open
;; -- so the suite leaves nothing behind.  Each unlink is guarded: a file that was
;; never created is the normal case here, not an error.
(define (remove-db! path)
  (for-each
    (lambda (suffix)
      (guard (e [#t #f]) (posix-unlink (string-append path suffix))))
    '("" "-journal" "-wal" "-shm")))

;; Insert one row on a raw connection, reporting whether it actually persisted.
(define (insert-row! db input mode)
  (let ([stmt (sqlite3-prepare db
                "INSERT INTO history (input, mode) VALUES (?1, ?2)")])
    (and stmt
         (begin
           (sqlite3-bind-text stmt 1 input)
           (sqlite3-bind-text stmt 2 mode)
           (let ([rc (sqlite3-step stmt)])
             (sqlite3-finalize stmt)
             (= rc SQLITE_DONE))))))

;; Build the database an earlier hafod would have left behind: the table as it was
;; first created (id, input, timestamp), the index, and the mode column arriving
;; afterwards as its own ALTER -- which is how a real history.db came by it -- with
;; no journal_mode PRAGMA anywhere in sight.  Every return is checked, so a fixture
;; that did not get built says so rather than being mistaken for a passing readback.
(define (build-rollback-history! path rows)
  (let ([db (sqlite3-open path)])
    (and db
         (let ([built
                 (and (= SQLITE_OK
                         (sqlite3-exec db
                           "CREATE TABLE IF NOT EXISTS history (
                              id INTEGER PRIMARY KEY AUTOINCREMENT,
                              input TEXT NOT NULL,
                              timestamp INTEGER NOT NULL DEFAULT (strftime('%s','now'))
                            )"))
                      (= SQLITE_OK
                         (sqlite3-exec db
                           "CREATE INDEX IF NOT EXISTS idx_history_ts ON history(timestamp)"))
                      (= SQLITE_OK
                         (sqlite3-exec db
                           "ALTER TABLE history ADD COLUMN mode TEXT DEFAULT 'scheme'"))
                      (for-all (lambda (row) (insert-row! db (car row) (cdr row)))
                               rows))])
           (sqlite3-close db)
           built))))

;; The journal mode recorded in the FILE, read on a connection of its own.  SQLite
;; keeps it in the database header -- which is what makes a WAL upgrade durable,
;; and what makes it readable back here after every handle has been closed.
(define (journal-mode-of path)
  (let* ([db (sqlite3-open path)]
         [stmt (and db (sqlite3-prepare db "PRAGMA journal_mode"))]
         [mode (if (and stmt (= (sqlite3-step stmt) SQLITE_ROW))
                   (sqlite3-column-text stmt 0)
                   #f)])
    (when stmt (sqlite3-finalize stmt))
    (when db (sqlite3-close db))
    mode))

(define (note . parts)
  (display "  note: ")
  (for-each display parts)
  (newline))

(test-begin "history-compat")

(define db-path (temp-db-path "compat"))

(define older-input "(display \"typed into a hafod of some months ago\")")
(define newer-input "ls -l /etc")

;; The file as it was left.  The older line went in first, so it holds the lower
;; id; the newer one was a shell command and was tagged as one.  Both modes are
;; known, and both are asserted -- a readback that lost the mode column would look
;; exactly like one that never had it.
(define rollback-rows
  (list (cons older-input "scheme")
        (cons newer-input "shell")))

;; sqlite3-open is the first call through the wrappers, so this is what loads the
;; shared object; sqlite3-loaded? can be trusted from here on.
(define built? (build-rollback-history! db-path rollback-rows))

(if (not (sqlite3-loaded?))
    (begin
      (note "libsqlite3 is unavailable; skipping the existing-history assertions")
      (remove-db! db-path))
    (begin

      ;; The fixture is really on disk: the table was created and both rows went
      ;; in.  Everything below reads this file, so a fixture that quietly failed to
      ;; build would turn every assertion after it into a question about nothing.
      (test-assert "the file an earlier hafod would have left is on disk" built?)

      ;; The premise, pinned.  What was just built really is a rollback-journal
      ;; database -- were it WAL already, the upgrade asserted at the end would be
      ;; satisfied without anything having upgraded anything.
      (test-equal "the file starts out on the rollback journal, as an earlier hafod left it"
        "delete"
        (journal-mode-of db-path))

      (let ([h (open-history db-path)])

        (test-equal "every entry in the existing file is still there"
          2
          (history-count h))

        (test-equal "the entries read back as they were written, oldest first"
          (list older-input newer-input)
          (list (history-ref h 0) (history-ref h 1)))

        (test-equal "each entry keeps the mode it was written with"
          '(scheme shell)
          (list (history-entry-mode h 0) (history-entry-mode h 1)))

        ;; The upgrade, observed on the file itself rather than taken on trust --
        ;; and observed WHILE THE SESSION IS LIVE, which is the only time it means
        ;; anything.  WAL is here so that a second session can read while this one
        ;; writes, so the question worth asking is whether the database is in WAL
        ;; while there is something to concurrently read; asking a file nobody has
        ;; open would only be reading a byte in a header that no longer governs
        ;; anything.  journal-mode-of opens a connection of its own, which is
        ;; exactly the second reader the mode exists to permit.
        (test-equal "opening it carries the file over to WAL"
          "wal"
          (journal-mode-of db-path))

        ;; Closed before the file is read back, so what is read is a settled
        ;; database and not one a live connection is still holding open.
        (history-close! h))

      ;; And what it leaves behind is one file.  Closing checkpoints the log back
      ;; into the database and takes the journal with it (see history-close!), so a
      ;; settled history is a single self-contained file -- not a database plus a
      ;; -wal holding the lines the user just typed, which is what Darwin's SQLite
      ;; leaves lying there when nobody puts it back.  The next open sets WAL again.
      (test-equal "the settled file is back on the rollback journal, log checkpointed in"
        "delete"
        (journal-mode-of db-path))

      (test-assert "no write-ahead log is left beside the settled file"
        (not (file-exists? (string-append db-path "-wal"))))

      (remove-db! db-path)))

(test-end)
