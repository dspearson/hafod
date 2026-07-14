;;; test/test-history-unwritten-entry.ss -- A line the database refused must not
;;; come back and retag the line before it.
;;;
;;; Every submitted line is added to the history and then tagged with the mode it
;;; classified as -- 'shell or 'scheme -- and the tag is an UPDATE naming the row
;;; the session inserted.  The two steps are separate, and the insert can fail:
;;; SQLITE_BUSY once a held write lock outlasts the busy timeout, SQLITE_FULL on a
;;; full disk, SQLITE_IOERR, SQLITE_READONLY.  Nothing raises when it does -- the
;;; step's return is not looked at -- so the tag runs regardless.
;;;
;;; What it must not do is land on somebody else's row.  sqlite3_last_insert_rowid
;;; is documented not to change on a failed insert, so after one the connection is
;;; still reporting the id of the row it inserted BEFORE -- an earlier line of this
;;; very session, already on the file and already correctly tagged.  A tag that
;;; takes that id at face value rewrites that older row with the mode of a line
;;; that never reached the database at all: a command remembered as "shell" comes
;;; back as "scheme", silently, and the line that did it is nowhere in the file.
;;;
;;; So the id has to be forgotten when the insert did not persist, and this suite
;;; pins the outcome: the earlier row keeps the mode it was written with.
;;;
;;; The refusal is made to happen rather than waited for.  A BEFORE INSERT trigger
;;; that RAISEs ABORT fails the insert at exactly the point a busy or a full disk
;;; does -- inside sqlite3_step, statement prepared and bound, nothing written --
;;; and it needs no second process and no timing to do it.  It is installed on a
;;; connection of its own, so the handle under test is left exactly as hafod has it.
;;;
;;; Three controls keep the observation honest.  The first pins that the earlier
;;; line reached the file carrying its own tag, so there is a correctly tagged row
;;; there to be rewritten.  The second pins that the refused line really did not
;;; reach the file -- that the trigger bit.  The third pins that it was still taken
;;; into memory, so the tag that follows it really did run against it: without that,
;;; an untouched earlier row would prove only that nothing happened at all.
;;;
;;; Entirely in-process -- one temporary file, no terminal -- so it runs the same
;;; way everywhere.  Degrades to a printed note where libsqlite3 cannot be loaded.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor history)
        (only (hafod editor sqlite3)
              sqlite3-open sqlite3-close sqlite3-exec sqlite3-prepare sqlite3-step
              sqlite3-finalize sqlite3-column-text sqlite3-loaded?
              SQLITE_OK SQLITE_ROW)
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

;; Read the history table back on a connection of its own -- not the handle under
;; test -- so what is asserted is what is on the FILE, in row-id order: the oldest
;; row first.  Each row comes back as (input . mode).
(define (read-rows path)
  (let* ([db (sqlite3-open path)]
         [stmt (and db (sqlite3-prepare db
                         "SELECT input, mode FROM history ORDER BY id"))])
    (let loop ([acc '()])
      (if (and stmt (= (sqlite3-step stmt) SQLITE_ROW))
          (loop (cons (cons (sqlite3-column-text stmt 0)
                            (sqlite3-column-text stmt 1))
                      acc))
          (begin
            (when stmt (sqlite3-finalize stmt))
            (when db (sqlite3-close db))
            (reverse acc))))))

;; The mode recorded against a given input, or #f when no such row is on the file.
(define (mode-of rows input)
  (cond [(assoc input rows) => cdr]
        [else #f]))

;; Make the database refuse the next insert, the way a real one is refused: the
;; step comes back something other than SQLITE_DONE and nothing is written.  A
;; trigger that ABORTs is the cheapest deterministic stand-in for the codes that do
;; it in the field -- busy, full, i/o error, read-only -- and it fails the statement
;; where they fail it, in the step.  Installed on a connection of its own, and its
;; return code is checked: a trigger that did not go in would leave the insert
;; succeeding and the whole suite proving nothing.
(define (refuse-inserts! path)
  (let ([db (sqlite3-open path)])
    (and db
         (let ([rc (sqlite3-exec db
                     "CREATE TRIGGER refuse_the_insert BEFORE INSERT ON history
                      BEGIN SELECT RAISE(ABORT, 'the database refused this insert'); END")])
           (sqlite3-close db)
           (= rc SQLITE_OK)))))

(define (note . parts)
  (display "  note: ")
  (for-each display parts)
  (newline))

(test-begin "history-unwritten-entry")

(define db-path (temp-db-path "unwritten-entry"))

;; open-history goes through the sqlite3 wrappers, and their shared object is
;; resolved on first use -- so this call is what loads it, and sqlite3-loaded? can
;; be trusted from here on.
(define h (open-history db-path))

(if (not (sqlite3-loaded?))
    (begin
      (note "libsqlite3 is unavailable; skipping the unwritten-entry assertions")
      (history-close! h)
      (remove-db! db-path))
    (let ([persisted "ls -la /etc"]
          [refused   "(display \"the database would not take this one\")"])

      ;; The session's first line.  It reaches the database, classifies as a shell
      ;; command, and is tagged as one -- an ordinary submission, start to finish.
      (history-add! h persisted)
      (history-set-last-mode! h 'shell)

      ;; Control: that row is on the file, carrying its own mode.  Everything below
      ;; asks whether a LATER line can reach back and rewrite this row, so the row
      ;; has to be there, correctly tagged, before the question means anything.
      (test-equal "the line that reached the database is tagged with its own mode"
        (list (cons persisted "shell"))
        (read-rows db-path))

      ;; Control: the refusal is armed.  A trigger that failed to install would
      ;; leave the next insert going in perfectly well, and the assertions below
      ;; would pass on any tree at all.
      (test-assert "the database is made to refuse the next insert"
        (refuse-inserts! db-path))

      ;; The next line.  The insert is refused; history-add! does not look at what
      ;; the step returned, so the line goes into the session's memory regardless
      ;; and the tag that follows every submission runs as it always does.
      (history-add! h refused)
      (history-set-last-mode! h 'scheme)

      ;; Control: the refused line really is not on the file -- the trigger bit.  A
      ;; line that had quietly gone in would make the observation below meaningless.
      (test-equal "the line the database refused never reached the file"
        (list persisted)
        (map car (read-rows db-path)))

      ;; Control: it WAS taken into memory, so the session tagged it -- the tag ran,
      ;; and had an entry to run against.  Without this, an untouched earlier row
      ;; would say only that nothing happened at all.
      (test-equal "the refused line is still held in memory, so it was tagged like any other"
        2
        (history-count h))

      ;; The observation.  The session's own earlier row keeps the mode it was
      ;; written with.  Before the fix, the refused insert left the previous row id
      ;; in place, the tag took it for its own, and the shell command on the file
      ;; came back as "scheme" -- rewritten by a line that is not in the database.
      (test-equal "a line that never reached the database does not retag the line before it"
        "shell"
        (mode-of (read-rows db-path) persisted))

      (history-close! h)
      (remove-db! db-path)))

(test-end)
