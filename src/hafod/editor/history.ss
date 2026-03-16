;;; (hafod editor history) -- SQLite-backed persistent command history
;;; Stores multi-line input with timestamps.  Provides Up/Down navigation
;;; with an in-memory cache of recent entries.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor history)
  (export open-history history? history-add! history-close!
          history-prev history-next history-reset-nav!
          history-save-input! history-saved-input
          history-cursor history-cursor-set!
          history-entries history-entry-mode
          history-set-last-mode!
          history-search-backward history-prefix-search-backward
          string-prefix?)
  (import (chezscheme)
          (hafod editor sqlite3)
          (hafod fuzzy))

  ;; History record:
  ;;   db      — SQLite database handle (or #f if unavailable)
  ;;   entries — vector of past inputs (strings), most recent last
  ;;   modes   — parallel vector of mode symbols ('scheme or 'shell), same length as entries
  ;;   cursor  — navigation index (-1 = at bottom / current input)
  ;;   saved   — saved current input when navigating away from bottom
  (define-record-type history
    (fields (mutable db)
            (mutable entries)
            (mutable modes)
            (mutable cursor)
            (mutable saved))
    (protocol (lambda (new)
                (lambda (db entries modes)
                  (new db entries modes -1 "")))))

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
           (sqlite3-exec db "CREATE INDEX IF NOT EXISTS idx_history_ts ON history(timestamp)")
           ;; Migrate: add mode column if missing (idempotent — ALTER TABLE
           ;; errors silently if column already exists)
           (guard (e [#t (void)])
             (sqlite3-exec db "ALTER TABLE history ADD COLUMN mode TEXT DEFAULT 'scheme'")))
         (let-values ([(entries modes) (if db (load-entries db) (values '#() '#()))])
           (make-history db entries modes)))]))

  ;; Load recent entries from DB into two vectors (oldest first):
  ;; entries (strings) and modes (symbols).
  (define (load-entries db)
    (let ([stmt (sqlite3-prepare db
                  (string-append
                    "SELECT input, mode FROM history ORDER BY id DESC LIMIT "
                    (number->string max-history)))])
      (if stmt
          (let loop ([inputs '()] [modes '()])
            (let ([rc (sqlite3-step stmt)])
              (cond
                [(= rc SQLITE_ROW)
                 (let ([input (sqlite3-column-text stmt 0)]
                       [mode-str (or (sqlite3-column-text stmt 1) "scheme")])
                   (loop (cons input inputs)
                         (cons (if (string=? mode-str "shell") 'shell 'scheme)
                               modes)))]
                [else
                 (sqlite3-finalize stmt)
                 (values (list->vector inputs)
                         (list->vector modes))])))
          (values '#() '#()))))

  ;; Add an entry to history (both DB and in-memory).
  ;; Skips empty strings and duplicates of the most recent entry.
  ;; Mode defaults to 'scheme; interactive.ss calls history-set-last-mode!
  ;; after classification to correct it.
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
                            "INSERT INTO history (input, mode) VALUES (?1, ?2)")])
                (when stmt
                  (sqlite3-bind-text stmt 1 input)
                  (sqlite3-bind-text stmt 2 "scheme")
                  (sqlite3-step stmt)
                  (sqlite3-finalize stmt)))))
          ;; Append to in-memory vectors
          (let* ([old (history-entries h)]
                 [old-m (history-modes h)]
                 [len (vector-length old)]
                 [new (make-vector (+ len 1))]
                 [new-m (make-vector (+ len 1))])
            (let copy ([i 0])
              (when (< i len)
                (vector-set! new i (vector-ref old i))
                (vector-set! new-m i (vector-ref old-m i))
                (copy (+ i 1))))
            (vector-set! new len input)
            (vector-set! new-m len 'scheme)
            (history-entries-set! h new)
            (history-modes-set! h new-m))))))

  ;; Update the mode of the most recent history entry.
  ;; Called by interactive.ss after input classification.
  (define (history-set-last-mode! h mode)
    (let* ([modes (history-modes h)]
           [len (vector-length modes)])
      (when (> len 0)
        (vector-set! modes (- len 1) mode)
        ;; Update DB
        (let ([db (history-db h)])
          (when db
            (let ([stmt (sqlite3-prepare db
                          "UPDATE history SET mode = ?1 WHERE id = (SELECT MAX(id) FROM history)")])
              (when stmt
                (sqlite3-bind-text stmt 1 (symbol->string mode))
                (sqlite3-step stmt)
                (sqlite3-finalize stmt))))))))

  ;; Look up the mode for a given history entry index.
  (define (history-entry-mode h idx)
    (let ([modes (history-modes h)])
      (if (and (>= idx 0) (< idx (vector-length modes)))
          (vector-ref modes idx)
          'scheme)))

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

  ;; ======================================================================
  ;; Search helpers
  ;; ======================================================================

  ;; Substring search: return #t if needle is found anywhere in haystack.
  (define (string-contains haystack needle)
    (let ([hlen (string-length haystack)]
          [nlen (string-length needle)])
      (cond
        [(= nlen 0) #t]
        [(> nlen hlen) #f]
        [else
         (let loop ([i 0])
           (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? needle (substring haystack i (+ i nlen))) #t]
             [else (loop (+ i 1))]))])))

  ;; Prefix check: return #t if str starts with prefix.
  (define (string-prefix? prefix str)
    (let ([plen (string-length prefix)]
          [slen (string-length str)])
      (and (<= plen slen)
           (string=? prefix (substring str 0 plen)))))

  ;; Search backward through history entries for a fuzzy match.
  ;; h: history object, query: search string, start-idx: index to start from (inclusive).
  ;; Returns the index of the first matching entry, or #f.
  (define (history-search-backward h query start-idx)
    (let ([entries (history-entries h)])
      (let loop ([i start-idx])
        (cond
          [(< i 0) #f]
          [(fuzzy-match query (vector-ref entries i)) i]
          [else (loop (- i 1))]))))

  ;; Search backward through history entries for a prefix match.
  ;; h: history object, prefix: prefix string, start-idx: index to start from (inclusive).
  ;; Returns the index of the first matching entry, or #f.
  (define (history-prefix-search-backward h prefix start-idx)
    (let ([entries (history-entries h)])
      (let loop ([i start-idx])
        (cond
          [(< i 0) #f]
          [(string-prefix? prefix (vector-ref entries i)) i]
          [else (loop (- i 1))]))))

) ; end library
