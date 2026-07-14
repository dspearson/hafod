;;; test/test-history-save-warning.ss -- Say so, once, when the history cannot be saved.
;;;
;;; history-add! prepares an INSERT, binds it, steps it -- and does not look at what
;;; the step returned.  So a database that refuses the write refuses it in silence:
;;; a full disk, an i/o error, a read-only file, a write lock somebody has held past
;;; the busy timeout.  The line stays in the session's memory and never reaches the
;;; file, and the user is told nothing.  They find out months later, when the history
;;; they went looking for is not there.
;;;
;;; The fix is a warning, and the shape of it is the point.  A shell must not die
;;; because its history could not be written -- the history is an accessory to the
;;; work, not the work -- so nothing is raised and the REPL carries on.  Nor may it
;;; nag: a database that refuses one insert refuses the next, and a line per command
;;; would make the shell unusable at exactly the moment it is trying to be helpful.
;;; So: once per session, on the way past, to standard error.  Then quiet.
;;;
;;; This suite pins all three -- that it warns, that it warns ONCE across repeated
;;; failures, and that the session is otherwise untouched: the entries are still in
;;; memory and history navigation still walks them.  The happy path is checked to be
;;; silent first, so a warning that fired on every add could not pass by warning
;;; "once" out of a bucket that was already full.
;;;
;;; The refusal is made to happen rather than waited for.  A BEFORE INSERT trigger
;;; that RAISEs ABORT fails the insert exactly where a full disk fails it -- inside
;;; sqlite3_step, statement prepared and bound, nothing written -- and needs no
;;; second process and no timing to do it.  It is installed on a connection of its
;;; own, so the handle under test is left as hafod has it.
;;;
;;; The warning is read by rebinding the console error port to a string port, which
;;; is where hafod's own diagnostics go and is a parameter like any other.  Entirely
;;; in-process -- one temporary file, no terminal -- so it runs the same way
;;; everywhere.  Degrades to a printed note where libsqlite3 cannot be loaded.
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

;; Remove a temporary database and any sidecar SQLite may have written beside it.
;; Each unlink is guarded: a file that was never created is the normal case here.
(define (remove-db! path)
  (for-each
    (lambda (suffix)
      (guard (e [#t #f]) (posix-unlink (string-append path suffix))))
    '("" "-journal" "-wal" "-shm")))

;; The inputs on the FILE, oldest first, read back on a connection of its own -- not
;; the handle under test -- so what is asserted is what was persisted, not what the
;; session remembers.
(define (persisted-inputs path)
  (let* ([db (sqlite3-open path)]
         [stmt (and db (sqlite3-prepare db "SELECT input FROM history ORDER BY id"))])
    (let loop ([acc '()])
      (if (and stmt (= (sqlite3-step stmt) SQLITE_ROW))
          (loop (cons (sqlite3-column-text stmt 0) acc))
          (begin
            (when stmt (sqlite3-finalize stmt))
            (when db (sqlite3-close db))
            (reverse acc))))))

;; Make the database refuse every insert from here on, the way a real one refuses:
;; the step comes back something other than SQLITE_DONE and nothing is written.  Its
;; return code is checked -- a trigger that did not go in would leave the inserts
;; succeeding and the whole suite proving nothing.
(define (refuse-inserts! path)
  (let ([db (sqlite3-open path)])
    (and db
         (let ([rc (sqlite3-exec db
                     "CREATE TRIGGER refuse_the_insert BEFORE INSERT ON history
                      BEGIN SELECT RAISE(ABORT, 'the database refused this insert'); END")])
           (sqlite3-close db)
           (= rc SQLITE_OK)))))

;; Run a thunk with the console error port -- where hafod's diagnostics go -- bound
;; to a string, and hand back what was written to it.
(define (stderr-of thunk)
  (let ([sp (open-output-string)])
    (parameterize ([console-error-port sp])
      (thunk))
    (get-output-string sp)))

;; How many times `needle` occurs in `haystack`.  The whole question here is one
;; versus none versus one-per-command, so it is counted, not merely looked for.
(define (occurrences haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (if (= nlen 0)
        0
        (let loop ([i 0] [n 0])
          (cond
            [(> (+ i nlen) hlen) n]
            [(string=? needle (substring haystack i (+ i nlen)))
             (loop (+ i nlen) (+ n 1))]
            [else (loop (+ i 1) n)])))))

(define (note . parts)
  (display "  note: ")
  (for-each display parts)
  (newline))

(test-begin "history-save-warning")

(define db-path (temp-db-path "save-warning"))

;; open-history goes through the sqlite3 wrappers, and their shared object is
;; resolved on first use -- so this call is what loads it, and sqlite3-loaded? can be
;; trusted from here on.
(define h (open-history db-path))

;; The substance of the warning, which is counted rather than searched for.
(define the-complaint "history could not be saved")

(if (not (sqlite3-loaded?))
    (begin
      (note "libsqlite3 is unavailable; skipping the save-warning assertions")
      (history-close! h)
      (remove-db! db-path))
    (let ([persisted "grep -r TODO src"]
          [refused-1 "make test"]
          [refused-2 "git status"])

      ;; Control: a line the database took is written without a word said about it.
      ;; A shell that complained on every command would otherwise satisfy "warns
      ;; once" below out of a bucket that was already full before the failures began.
      (test-equal "a line the database accepts is saved without comment"
        ""
        (stderr-of (lambda () (history-add! h persisted))))

      ;; Control: the refusal is armed.  A trigger that failed to install would leave
      ;; the inserts below going in perfectly well, and the assertions would be
      ;; asserting nothing.
      (test-assert "the database is made to refuse every insert from here"
        (refuse-inserts! db-path))

      ;; Two commands, both refused.  This is the shape of the real thing: a database
      ;; that cannot take one insert cannot take the next either, and the user is
      ;; still typing.
      (let ([complaints (stderr-of (lambda ()
                                     (history-add! h refused-1)
                                     (history-add! h refused-2)))])

        ;; The observation.  Told once -- not never, which is the bug, and not twice,
        ;; which is a shell that shouts over its own output for the rest of the day.
        (test-equal "the user is warned once, and only once, when the history cannot be saved"
          1
          (occurrences complaints the-complaint))

        ;; The warning is hafod's, says what has happened and what follows from it,
        ;; and ends its line -- a diagnostic without a newline runs into the prompt
        ;; that comes after it.
        (test-assert "the warning names hafod, says the history is now memory-only, and ends its line"
          (and (> (occurrences complaints "hafod:") 0)
               (> (occurrences complaints "in-memory") 0)
               (> (string-length complaints) 0)
               (char=? (string-ref complaints (- (string-length complaints) 1)) #\newline))))

      ;; Nothing was raised and nothing was lost: the shell carried on, and both
      ;; refused lines are in the session's memory where the user can reach them.
      (test-equal "the refused lines are still in the session's history"
        (list 3 persisted refused-1 refused-2)
        (list (history-count h)
              (history-ref h 0) (history-ref h 1) (history-ref h 2)))

      ;; And the history still works as a history -- the failure did not leave the
      ;; record in a state that breaks the next Up press.
      (test-equal "history navigation still walks the entries after a failed save"
        (list refused-2 refused-1)
        (list (history-prev h) (history-prev h)))

      ;; Control: the refused lines really are not on the file -- the trigger bit, and
      ;; the warning was warning about something that actually happened.
      (test-equal "only the line the database accepted reached the file"
        (list persisted)
        (persisted-inputs db-path))

      (history-close! h)
      (remove-db! db-path)))

(test-end)
