;;; (hafod editor history) -- SQLite-backed persistent command history
;;; Stores multi-line input with timestamps.  Provides Up/Down navigation
;;; with an in-memory cache of recent entries.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor history)
  (export open-history history? history-add! history-close!
          history-prev history-next history-reset-nav!
          history-save-input! history-saved-input
          history-cursor)
  (import (chezscheme)
          (hafod editor sqlite3))

  ;; History record:
  ;;   db      — SQLite database handle (or #f if unavailable)
  ;;   entries — vector of past inputs, most recent last
  ;;   cursor  — navigation index (-1 = at bottom / current input)
  ;;   saved   — saved current input when navigating away from bottom
  (define-record-type history
    (fields (mutable db)
            (mutable entries)
            (mutable cursor)
            (mutable saved))
    (protocol (lambda (new)
                (lambda (db entries)
                  (new db entries -1 "")))))

  (define max-history 10000)

  ;; Default path: ~/.hafod_history.db
  (define (default-history-path)
    (let ([home (or (getenv "HOME") ".")])
      (string-append home "/.hafod_history.db")))

  ;; Open or create the history database.
  ;; Returns a history object (possibly with db=#f if SQLite unavailable).
  (define open-history
    (case-lambda
      [() (open-history (default-history-path))]
      [(path)
       (let ([db (sqlite3-open path)])
         (when db
           (sqlite3-exec db
             "CREATE TABLE IF NOT EXISTS history (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                input TEXT NOT NULL,
                timestamp INTEGER NOT NULL DEFAULT (strftime('%s','now'))
              )")
           (sqlite3-exec db "CREATE INDEX IF NOT EXISTS idx_history_ts ON history(timestamp)"))
         (let ([entries (if db (load-entries db) '#())])
           (make-history db entries)))]))

  ;; Load recent entries from DB into a vector (oldest first).
  (define (load-entries db)
    (let ([stmt (sqlite3-prepare db
                  (string-append
                    "SELECT input FROM history ORDER BY id DESC LIMIT "
                    (number->string max-history)))])
      (if stmt
          (let loop ([acc '()])
            (let ([rc (sqlite3-step stmt)])
              (cond
                [(= rc SQLITE_ROW)
                 (loop (cons (sqlite3-column-text stmt 0) acc))]
                [else
                 (sqlite3-finalize stmt)
                 (list->vector acc)])))  ; acc is reversed, oldest first
          '#())))

  ;; Add an entry to history (both DB and in-memory).
  ;; Skips empty strings and duplicates of the most recent entry.
  (define (history-add! h input)
    (when (and (string? input)
               (> (string-length input) 0))
      (let ([entries (history-entries h)])
        (unless (and (> (vector-length entries) 0)
                     (string=? input (vector-ref entries (- (vector-length entries) 1))))
          ;; Persist to DB
          (let ([db (history-db h)])
            (when db
              (let ([stmt (sqlite3-prepare db
                            "INSERT INTO history (input) VALUES (?1)")])
                (when stmt
                  (sqlite3-bind-text stmt 1 input)
                  (sqlite3-step stmt)
                  (sqlite3-finalize stmt)))))
          ;; Append to in-memory vector
          (let* ([old (history-entries h)]
                 [len (vector-length old)]
                 [new (make-vector (+ len 1))])
            (let copy ([i 0])
              (when (< i len)
                (vector-set! new i (vector-ref old i))
                (copy (+ i 1))))
            (vector-set! new len input)
            (history-entries-set! h new))))))

  ;; Navigate to previous (older) entry.
  ;; Returns the history entry string, or #f if at the oldest.
  (define (history-prev h)
    (let* ([entries (history-entries h)]
           [len (vector-length entries)]
           [cur (history-cursor h)])
      (cond
        [(= len 0) #f]
        [(= cur -1)
         ;; First upward press: go to most recent entry
         (let ([idx (- len 1)])
           (history-cursor-set! h idx)
           (vector-ref entries idx))]
        [(> cur 0)
         ;; Move to older entry
         (let ([idx (- cur 1)])
           (history-cursor-set! h idx)
           (vector-ref entries idx))]
        [else #f])))  ; already at oldest

  ;; Navigate to next (newer) entry.
  ;; Returns the history entry string, or the saved input if at bottom.
  ;; Returns #f if already at bottom.
  (define (history-next h)
    (let* ([entries (history-entries h)]
           [len (vector-length entries)]
           [cur (history-cursor h)])
      (cond
        [(= cur -1) #f]  ; already at bottom
        [(< cur (- len 1))
         ;; Move to newer entry
         (let ([idx (+ cur 1)])
           (history-cursor-set! h idx)
           (vector-ref entries idx))]
        [else
         ;; At most recent → return to current input
         (history-cursor-set! h -1)
         (history-saved h)])))

  ;; Reset navigation cursor to bottom (call after submitting).
  (define (history-reset-nav! h)
    (history-cursor-set! h -1)
    (history-saved-set! h ""))

  ;; Save the current (unsaved) input before navigating away.
  (define (history-save-input! h input)
    (history-saved-set! h input))

  ;; Get the saved input.
  (define (history-saved-input h)
    (history-saved h))

  ;; Close the history database.
  (define (history-close! h)
    (let ([db (history-db h)])
      (when db
        (sqlite3-close db)
        (history-db-set! h #f))))

) ; end library
