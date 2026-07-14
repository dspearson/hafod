;;; (hafod editor history) -- SQLite-backed persistent command history
;;; Stores multi-line input with timestamps.  Provides Up/Down navigation
;;; with an in-memory cache of recent entries.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor history)
  (export open-history history? history-add! history-close!
          history-prev history-next history-reset-nav!
          history-save-input! history-saved-input
          history-cursor history-cursor-set!
          history-entries history-count history-ref history-entry-mode
          history-set-last-mode!
          history-search-backward history-prefix-search-backward
          string-prefix?)
  (import (chezscheme)
          (hafod editor sqlite3)
          (hafod fuzzy)
          (only (hafod exit-hooks) add-exit-hook!)
          (only (hafod internal posix-file) posix-chmod)
          (only (hafod srfi-13) string-prefix?))

  ;; History record:
  ;;   db          — SQLite database handle (or #f if unavailable)
  ;;   entry-store — growable backing vector of past inputs (strings), most
  ;;                 recent last.  Only the first `count` slots are live; the
  ;;                 tail is spare capacity that history-add! fills before it
  ;;                 next has to double the store.
  ;;   mode-store  — parallel growable vector of mode symbols ('scheme or
  ;;                 'shell); live slots track entry-store one-for-one.
  ;;   count       — number of live entries (O(1)); may be < the store length.
  ;;   cursor      — navigation index (-1 = at bottom / current input)
  ;;   saved       — saved current input when navigating away from bottom
  ;;   last-rowid  — id of the row THIS handle last inserted, or 0 if it has
  ;;                 inserted none.  Internal: it names a row in the database
  ;;                 for history-set-last-mode! and is of no use to a caller,
  ;;                 so its accessors are not exported.  Row ids are per
  ;;                 connection, which is the whole point — see
  ;;                 history-set-last-mode!.
  ;;   warned      — #t once this session has told the user their history is
  ;;                 not reaching the disk.  Internal, and per record rather
  ;;                 than per module: "once" means once a session, and a
  ;;                 session is a history.  See report-save-failure!.
  (define-record-type history
    (fields (mutable db)
            (mutable entry-store)
            (mutable mode-store)
            (mutable count)
            (mutable cursor)
            (mutable saved)
            (mutable last-rowid)
            (mutable warned))
    (protocol (lambda (new)
                ;; open-history still passes right-sized vectors; the store
                ;; simply starts exactly full (count = length) and may
                ;; over-allocate on a later append.
                (lambda (db entries modes)
                  (new db entries modes (vector-length entries) -1 "" 0 #f)))))

  (define max-history 10000)

  ;; O(1) positional entry access (0-based, most recent last).  Callers
  ;; guarantee 0 <= i < (history-count h) via history-count, so no bounds
  ;; check is needed on this hot path.
  (define (history-ref h i)
    (vector-ref (history-entry-store h) i))

  ;; O(1) mode access at the same index, under the same caller guarantee.
  (define (history-mode-ref h i)
    (vector-ref (history-mode-store h) i))

  ;; A right-sized view of the live entries (length == count).  Reserved for
  ;; the one-shot / per-submission consumers (deduplicate-history and history
  ;; expansion) that legitimately want a plain vector; the per-keystroke
  ;; readers use history-count/history-ref and never allocate here.  When the
  ;; store is exactly full the backing vector already is the view, so it is
  ;; returned directly (both consumers only read it); otherwise a right-sized
  ;; copy of the live prefix is returned, dropping the spare-capacity tail.
  (define (history-entries h)
    (let* ([store (history-entry-store h)]
           [count (history-count h)]
           [capacity (vector-length store)])
      (if (= count capacity)
          store
          (let ([view (make-vector count)])
            (let loop ([i 0])
              (if (< i count)
                  (begin
                    (vector-set! view i (vector-ref store i))
                    (loop (+ i 1)))
                  view))))))

  ;; Default path: ~/.hafod_history.db
  (define (default-history-path)
    (let ([home (or (getenv "HOME") ".")])
      (string-append home "/.hafod_history.db")))

  ;; Keep the history file to the person whose history it is.
  ;;
  ;; Every line they typed is in here verbatim: the exported API key, the token
  ;; pasted into a curl header, the password that went onto a command line by
  ;; mistake.  SQLite creates a database 0666 & ~umask, which on the umask a
  ;; login shell actually hands out is 0644 -- readable by every account on the
  ;; machine.  bash, zsh and fish all keep their history at 0600, and so does
  ;; this.  Applied on every open, not only the one that creates the file, so a
  ;; history written before this existed is repaired the next time it is opened
  ;; rather than staying exposed for as long as the user keeps it.
  ;;
  ;; It goes before the first PRAGMA, and the order is the point.  The -wal and
  ;; -shm hold the same commands -- the most recent ones, sitting there unmerged
  ;; until a checkpoint -- and SQLite gives a sidecar it creates the mode of the
  ;; DATABASE FILE as it finds it at that moment.  Tighten the database before
  ;; anything writes, and the sidecars are born owner-only; tighten it after, and
  ;; they keep the 0644 they inherited from the file as it was.
  ;;
  ;; Two things it must not do.  It must not fail on ":memory:", which names no
  ;; file at all -- so that is not attempted.  And it must not take the shell down
  ;; with it: a history is an accessory to the work, not the work, and a chmod
  ;; refused (a database on a filesystem with no permission bits, a file owned by
  ;; somebody else) is not a reason to lose the session.  posix-chmod raises on
  ;; failure, so the failure is caught and the shell carries on with a file it
  ;; could not tighten -- as it did before any of this.
  (define (restrict-to-owner! path)
    (unless (string=? path ":memory:")
      (guard (e [#t (void)])
        (posix-chmod path #o600))))

  ;; Open or create the history database.
  ;; Returns a history object (possibly with db=#f if SQLite unavailable).
  (define open-history
    (case-lambda
      [() (open-history (default-history-path))]
      [(path)
       (let ([db (sqlite3-open path)])
         (when db
           ;; Nobody else's business.  Done first, while the database is still the
           ;; only file there is: the sidecars below take their mode from this one
           ;; as they are created.
           (restrict-to-owner! path)
           ;; Configure the database for the concurrent sessions it actually has,
           ;; before anything is asked of it.
           ;;
           ;; WAL lets a reader run alongside a writer instead of taking turns,
           ;; and the busy_timeout gives a writer that meets a lock somebody else
           ;; is holding five seconds in which to get it -- rather than none at
           ;; all, which is what the default 0 means.  Without the timeout SQLite
           ;; reports SQLITE_BUSY the moment it finds the write lock taken, and
           ;; history-add! does not look at what its step returned: the insert is
           ;; dropped, nothing raises, nothing is printed, and the line the user
           ;; typed is simply not in their history the next time they look for it.
           ;;
           ;; The journal mode is durable -- SQLite records it in the database
           ;; header -- so a history file written before this existed is carried
           ;; over to WAL the first time it is opened, with its rows untouched, and
           ;; the -wal / -shm files that then appear beside it are ordinary SQLite.
           ;; On an in-memory database the mode cannot be changed and the PRAGMA is
           ;; a documented no-op, which is why it is safe to issue unconditionally.
           ;;
           ;; The timeout goes FIRST, and the order is the point.  That carry-over
           ;; rewrites the database header, so it wants an exclusive lock and must
           ;; wait for any reader to finish with the file -- and the busy handler is
           ;; what does the waiting.  Issued the other way round, the conversion runs
           ;; while the timeout is still the default zero: it meets a session that is
           ;; merely reading, is refused on the spot, and -- its return discarded like
           ;; every other here -- this session then runs its whole life on the
           ;; rollback journal in the belief that it has WAL.  Nothing says so.  Set
           ;; the timeout before anything that can contend, and the conversion rides
           ;; out the moment somebody else is holding rather than losing WAL for the
           ;; session.
           (sqlite3-exec db "PRAGMA busy_timeout=5000")
           (sqlite3-exec db "PRAGMA journal_mode=WAL")
           (sqlite3-exec db
             "CREATE TABLE IF NOT EXISTS history (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                input TEXT NOT NULL,
                timestamp INTEGER NOT NULL DEFAULT (strftime('%s','now'))
              )")
           (sqlite3-exec db "CREATE INDEX IF NOT EXISTS idx_history_ts ON history(timestamp)")
           ;; Migrate: give a database written before the mode column existed one
           ;; now.  There is no ALTER TABLE ... IF NOT EXISTS, so this is run every
           ;; open and its return code is dropped on purpose: on the second and
           ;; every later open the column is already there, SQLite comes back
           ;; SQLITE_ERROR ("duplicate column name"), and that is the ordinary case
           ;; rather than a fault.  Nothing is caught here because nothing is
           ;; thrown -- sqlite3-exec reports status, it does not raise -- so a guard
           ;; around this would catch nothing and say, untruthfully, that it did.
           ;; Should the migration ever need to tell "already there" from a real
           ;; failure, it will have to read that return code.
           (sqlite3-exec db "ALTER TABLE history ADD COLUMN mode TEXT DEFAULT 'scheme'"))
         (let-values ([(entries modes) (if db (load-entries db) (values '#() '#()))])
           (let ([h (make-history db entries modes)])
             ;; Close the database on the way out.  hafod's exit runs the exit
             ;; hooks before it flushes and leaves, so a normal exit releases the
             ;; database rather than leaving it for the kernel to reclaim at
             ;; teardown — which is not the same thing, and hands SQLite no
             ;; chance to finish with the file.  history-close! clears the db
             ;; field, so a hook that somehow runs twice closes once.  Nothing is
             ;; registered when no database was opened: there is nothing to
             ;; close, and a run that never opens one (a script, say) leaves the
             ;; exit hooks as it found them.
             (when db
               (add-exit-hook! (lambda () (history-close! h))))
             h)))]))

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

  ;; The database would not take a write.  Say so, and carry on.
  ;;
  ;; A step that comes back anything but SQLITE_DONE wrote nothing: the disk is
  ;; full, the file is read-only, the i/o failed, or a lock somebody else is
  ;; holding outlasted the busy timeout.  Every one of those leaves the line in
  ;; this session's memory and not in the file, and every one of them used to
  ;; pass without a word — the step's return was read for the row id and not
  ;; otherwise looked at.  The user found out months later, when the command they
  ;; went back for was not there.
  ;;
  ;; Degrade loudly, but do not die.  Nothing is raised: a history is an accessory
  ;; to the work, not the work, and a shell that fell over because it could not
  ;; write its history would be worse than one that quietly did not.  The REPL is
  ;; untouched, the in-memory history still holds the line, and the session runs on.
  ;;
  ;; And say it ONCE.  A database that refuses one insert refuses the next: the
  ;; disk that is full stays full.  A line per command would put a complaint
  ;; between the user and every piece of output they asked for, for the rest of the
  ;; day — so the first failure speaks, the flag goes up, and the rest are silent.
  ;; The flag is on the record, so "once" means once per session, and a new shell
  ;; says it again rather than inheriting somebody else's silence.
  ;;
  ;; It goes to the console error port, which is where hafod's own diagnostics go
  ;; and is not what a redirection rebinds — this is the shell talking about itself,
  ;; not output of the command being run, and it belongs on the terminal even when
  ;; the command's own stderr has been sent elsewhere.  Flushed, so it lands before
  ;; the next prompt rather than after it.
  (define (report-save-failure! h)
    (unless (history-warned h)
      (history-warned-set! h #t)
      (let ([port (console-error-port)])
        (display "hafod: history could not be saved; continuing with in-memory history only.\n"
                 port)
        (flush-output-port port))))

  ;; Add an entry to history (both DB and in-memory).
  ;; Skips empty strings and duplicates of the most recent entry.
  ;; Mode defaults to 'scheme; interactive.ss calls history-set-last-mode!
  ;; after classification to correct it.
  (define (history-add! h input)
    (when (and (string? input)
               (> (string-length input) 0))
      (let ([count (history-count h)])
        (unless (and (> count 0)
                     (string=? input (history-ref h (- count 1))))
          ;; Persist to DB, and remember WHICH row was written.  The id is read
          ;; on the connection that did the insert, so it names this session's
          ;; own row even when another session writes into the same file in
          ;; behind us; history-set-last-mode! then updates that row and no
          ;; other.
          ;;
          ;; A step that did not run to completion persisted nothing, and the id
          ;; is FORGOTTEN when it does not — not merely left alone.  SQLite does
          ;; not change what sqlite3_last_insert_rowid reports on a failed
          ;; insert, so leaving the id in place leaves it naming the row this
          ;; session inserted BEFORE: one already on the file, and already
          ;; tagged with the mode it classified as.  history-set-last-mode! would
          ;; take that id for its own and rewrite that older row with the mode of
          ;; a line the database never took — a shell command remembered as
          ;; scheme, silently, and the line that did it nowhere in the file.
          ;; Zeroing it leaves no row of ours to name, which is exactly the case,
          ;; and the (> rowid 0) guard there then leaves the database alone.
          (let ([db (history-db h)])
            (when db
              (let ([stmt (sqlite3-prepare db
                            "INSERT INTO history (input, mode) VALUES (?1, ?2)")])
                (when stmt
                  (sqlite3-bind-text stmt 1 input)
                  (sqlite3-bind-text stmt 2 "scheme")
                  (let ([rc (sqlite3-step stmt)])
                    (sqlite3-finalize stmt)
                    (if (= rc SQLITE_DONE)
                        (history-last-rowid-set! h (sqlite3-last-insert-rowid db))
                        (begin
                          (history-last-rowid-set! h 0)
                          (report-save-failure! h))))))))
          ;; Append in amortised constant time: write into the next free slot
          ;; and bump the count.  Only when the store is full do we double it —
          ;; copying the live `count` elements once (the sole copy, amortised
          ;; across the appends that filled it).
          (when (= count (vector-length (history-entry-store h)))
            (let* ([old (history-entry-store h)]
                   [old-m (history-mode-store h)]
                   [capacity (vector-length old)]
                   [new-capacity (if (= capacity 0) 1 (* 2 capacity))]
                   [new (make-vector new-capacity)]
                   [new-m (make-vector new-capacity)])
              (let copy ([i 0])
                (when (< i count)
                  (vector-set! new i (vector-ref old i))
                  (vector-set! new-m i (vector-ref old-m i))
                  (copy (+ i 1))))
              (history-entry-store-set! h new)
              (history-mode-store-set! h new-m)))
          (vector-set! (history-entry-store h) count input)
          (vector-set! (history-mode-store h) count 'scheme)
          (history-count-set! h (+ count 1))))))

  ;; Update the mode of the most recent history entry.
  ;; Called by interactive.ss after input classification.
  ;;
  ;; The row is named by the id this handle recorded when it inserted it.  A
  ;; (SELECT MAX(id) FROM history) here would instead name whichever row the
  ;; FILE saw last — and with two sessions sharing one history database, that is
  ;; the row the OTHER session just wrote.  The tag would land on a stranger's
  ;; entry and silently rewrite it, with nothing raised and nothing to see until
  ;; that session next started and found its line remembered as the wrong mode.
  ;;
  ;; When this handle has no row of its own to name (last-rowid is 0), the
  ;; database is left alone.  That is the case whenever the last submission wrote
  ;; nothing: the input repeated the entry already held in memory, so history-add!
  ;; skipped the insert; or the insert was made and the database refused it, so
  ;; history-add! forgot the id rather than let this tag land on the row before.
  ;; The in-memory tag below is set regardless, outside the database update — so
  ;; what this session shows the user is unaffected either way.
  (define (history-set-last-mode! h mode)
    (let ([count (history-count h)])
      (when (> count 0)
        (vector-set! (history-mode-store h) (- count 1) mode)
        ;; Update DB
        (let ([db (history-db h)]
              [rowid (history-last-rowid h)])
          (when (and db (> rowid 0))
            (let ([stmt (sqlite3-prepare db
                          "UPDATE history SET mode = ?1 WHERE id = ?2")])
              (when stmt
                (sqlite3-bind-text stmt 1 (symbol->string mode))
                (sqlite3-bind-int64 stmt 2 rowid)
                ;; The other write path, and it was the muter of the two: this
                ;; step's return was not even read.  A tag that did not land
                ;; leaves the row on the file carrying the wrong mode, which is
                ;; the same failure to persist as a dropped insert and is
                ;; reported the same way -- once, then quietly.
                (let ([rc (sqlite3-step stmt)])
                  (sqlite3-finalize stmt)
                  (unless (= rc SQLITE_DONE)
                    (report-save-failure! h))))))))))

  ;; Look up the mode for a given history entry index.
  (define (history-entry-mode h idx)
    (if (and (>= idx 0) (< idx (history-count h)))
        (history-mode-ref h idx)
        'scheme))

  ;; Navigate to previous (older) entry.
  ;; Returns the history entry string, or #f if at the oldest.
  (define (history-prev h)
    (let ([len (history-count h)]
          [cur (history-cursor h)])
      (cond
        [(= len 0) #f]
        [(= cur -1)
         ;; First upward press: go to most recent entry
         (let ([idx (- len 1)])
           (history-cursor-set! h idx)
           (history-ref h idx))]
        [(> cur 0)
         ;; Move to older entry
         (let ([idx (- cur 1)])
           (history-cursor-set! h idx)
           (history-ref h idx))]
        [else #f])))  ; already at oldest

  ;; Navigate to next (newer) entry.
  ;; Returns the history entry string, or the saved input if at bottom.
  ;; Returns #f if already at bottom.
  (define (history-next h)
    (let ([len (history-count h)]
          [cur (history-cursor h)])
      (cond
        [(= cur -1) #f]  ; already at bottom
        [(< cur (- len 1))
         ;; Move to newer entry
         (let ([idx (+ cur 1)])
           (history-cursor-set! h idx)
           (history-ref h idx))]
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

  ;; Close the history database, taking the write-ahead log with it.
  ;;
  ;; The session runs in WAL (see the open path), so the lines just typed live in
  ;; a -wal file BESIDE the database rather than in it.  Closing the connection is
  ;; not by itself enough to be rid of that.  SQLite unlinks the -wal only when the
  ;; last connection to close can take the database exclusively -- and Darwin's
  ;; SQLite keeps the sidecars even then (its own sqlite3(1) does the same), so on
  ;; macOS the commands a user typed were left sitting in ~/.hafod_history.db-wal
  ;; after every exit, where strings(1) reads them straight out.
  ;;
  ;; Switching the journal back to DELETE checkpoints the log INTO the database and
  ;; unlinks it, so what the user is left holding is one file that is the whole of
  ;; their history, with nothing legible lying beside it.  That is worth doing on
  ;; every platform, not just the one that showed the bug: it is what makes the
  ;; database self-contained the moment the session ends.
  ;;
  ;; Best-effort by design.  The pragma needs the database exclusively, so while
  ;; another session still holds it open the switch simply does not happen: the
  ;; mode stays WAL, sqlite3_exec reports the failure in its return code, and the
  ;; last session out does the checkpoint instead.  The open path sets WAL again
  ;; next time.  Nothing here may keep us from closing -- this runs from an exit
  ;; hook, and an unusable database must not cost the user a clean exit.
  (define (history-close! h)
    (let ([db (history-db h)])
      (when db
        (guard (e [#t #f])
          (sqlite3-exec db "PRAGMA journal_mode=DELETE"))
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

  ;; string-prefix? imported from (hafod srfi-13)

  ;; Search backward through history entries for a fuzzy match.
  ;; h: history object, query: search string, start-idx: index to start from (inclusive).
  ;; Returns the index of the first matching entry, or #f.
  (define (history-search-backward h query start-idx)
    (let loop ([i start-idx])
      (cond
        [(< i 0) #f]
        [(fuzzy-match query (history-ref h i)) i]
        [else (loop (- i 1))])))

  ;; Search backward through history entries for a prefix match.
  ;; h: history object, prefix: prefix string, start-idx: index to start from (inclusive).
  ;; Returns the index of the first matching entry, or #f.
  (define (history-prefix-search-backward h prefix start-idx)
    (let loop ([i start-idx])
      (cond
        [(< i 0) #f]
        [(string-prefix? prefix (history-ref h i)) i]
        [else (loop (- i 1))])))

) ; end library
