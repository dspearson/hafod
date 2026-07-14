;;; test/test-history-own-row.ss -- A session that tags its history entry with an
;;; eval mode must tag the row IT wrote, not whichever row is newest in the file.
;;;
;;; Two hafod sessions share one history database, and each is a separate SQLite
;;; connection to the same file.  Their writes interleave: session one submits a
;;; line, session two submits a line, and session one then tags its entry with
;;; the mode its input classified as.  A mode update that names its row with
;;; (SELECT MAX(id) FROM history) names whatever row the FILE saw last -- which,
;;; once another session has written in behind you, is that session's row.  The
;;; tag then lands on a stranger's entry and silently rewrites it.  Nothing
;;; errors; the other session simply finds its line remembered as the wrong mode
;;; on its next start.
;;;
;;; The row id is per connection (sqlite3_last_insert_rowid reports the row THIS
;;; connection inserted), so a writer can name its own row and update that.  This
;;; suite pins the outcome: session one's tag lands on session one's row, and
;;; session two's row is exactly as session two left it.
;;;
;;; The first assertion is the control, and it is what keeps the other two
;;; honest: it reads the file back in id order and pins that session two's row IS
;;; the newest one.  Were that not so, MAX(id) would happen to name session one's
;;; row anyway and the proof would pass while proving nothing.
;;;
;;; Both handles are closed before the file is read back, so the readback sees a
;;; settled database rather than one two live connections are still holding open.
;;;
;;; Entirely in-process -- two connections, one temporary file, no terminal -- so
;;; it runs the same way everywhere.  Degrades to a printed note where libsqlite3
;;; cannot be loaded at all.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod editor history)
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

;; Read the history table back on a connection of its own -- neither session's --
;; so what is asserted is what is on the FILE, in row-id order: the oldest row
;; first, the newest last.  Each row comes back as (input . mode).  A mode read
;; back as #f would be a NULL column, and is deliberately not papered over with a
;; default here: the writers under test always bind the column.
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

(define (note . parts)
  (display "  note: ")
  (for-each display parts)
  (newline))

(test-begin "history-own-row")

(define db-path (temp-db-path "own-row"))

;; open-history goes through the sqlite3 wrappers, and their shared object is
;; resolved on first use -- so this call is what loads it, and sqlite3-loaded?
;; can be trusted from here on.
(define session-one (open-history db-path))

(if (not (sqlite3-loaded?))
    (begin
      (note "libsqlite3 is unavailable; skipping the own-row assertions")
      (history-close! session-one)
      (remove-db! db-path))
    (let ([session-two (open-history db-path)]
          [input-one "(display \"session one\")"]
          [input-two "(display \"session two\")"])

      ;; The interleave.  Session one submits, then session two submits: the
      ;; file's newest row now belongs to session two, not to session one.
      (history-add! session-one input-one)
      (history-add! session-two input-two)

      ;; Session one classified its input as a shell command and tags its entry
      ;; accordingly -- the tag belongs on session one's row and nowhere else.
      ;; history-add! writes every entry as "scheme", so session two's row is
      ;; "scheme" unless something rewrote it.
      (history-set-last-mode! session-one 'shell)

      (history-close! session-one)
      (history-close! session-two)

      (let ([rows (read-rows db-path)])

        ;; The control.  Both entries are on the file, oldest first -- so session
        ;; two's row is the newest, and a MAX(id) update issued by session one
        ;; would land on it.  Without this, an accidental ordering could make the
        ;; two assertions below pass on a tree that still clobbers.
        (test-equal
          "both sessions' entries are on the file, the other session's written last"
          (list input-one input-two)
          (map car rows))

        (test-equal "a session's mode tag lands on the entry that session wrote"
          "shell"
          (mode-of rows input-one))

        (test-equal "another session's entry is left exactly as that session wrote it"
          "scheme"
          (mode-of rows input-two)))

      (remove-db! db-path)))

(test-end)
