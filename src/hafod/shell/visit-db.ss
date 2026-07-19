;;; (hafod shell visit-db) -- SQLite-backed directory visit database.
;;;
;;; Records every directory change and ranks directories by a zoxide-style
;;; frecency score -- visit frequency weighted by recency -- so a jump command
;;; can take the user to the directory they most likely mean.  The database
;;; lifecycle mirrors (hafod editor history) verbatim: owner-only 0600 set
;;; before the first write, WAL journal, folded back into a single file and
;;; closed on exit.  It reuses the (hafod editor sqlite3) FFI wholesale and
;;; adds no foreign procedure of its own.  It deliberately imports none of the
;;; interactive editor / finder / terminal stack, so it stays unit-testable in
;;; process.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod shell visit-db)
  (export open-visit-db visitdb? visit-record! visit-query-best
          visit-candidates visit-db-close! frecency-score visit-now
          ensure-visit-db! visit-recording? visit-db-path)
  (import (chezscheme)
          (hafod editor sqlite3)
          (only (hafod exit-hooks) add-exit-hook!)
          (only (hafod internal posix-file) posix-chmod)
          (only (hafod srfi-13) string-contains string-contains-ci))

  ;; The handle wraps the raw SQLite connection so a visit can be recorded,
  ;; queried and closed through one value:
  ;;   db      -- the connection, or #f when libsqlite3 is absent, the open
  ;;              failed, or the handle has since been closed.  Every entry
  ;;              point treats a #f db as "do nothing", which is what keeps a
  ;;              missing or unwritable database from ever reaching navigation.
  ;;   warned  -- #t once this session has told the user the visit database is
  ;;              not taking writes.  Per handle, so "once" means once a session
  ;;              (see report-save-failure!).
  (define-record-type visitdb
    (fields (mutable db) (mutable warned)))

  ;; The one live connection, or #f.  Guards every entry point and tolerates a
  ;; non-handle argument, so a caller that passes #f -- or a handle whose db has
  ;; already been closed -- gets the quiet no-op rather than a raise.
  (define (live-db vdb)
    (and (visitdb? vdb) (visitdb-db vdb)))

  ;; Default path: ~/.hafod_visits.db -- a HOME dotfile beside ~/.hafod_history.db.
  ;; HOME always exists, so unlike an XDG data directory there is no parent to
  ;; create and no "the directory is absent, so the open silently fails forever"
  ;; corner to guard; it keeps the visit database consistent with the history
  ;; database it mirrors in every other respect.
  (define (default-visit-db-path)
    (let ([home (or (getenv "HOME") ".")])
      (string-append home "/.hafod_visits.db")))

  ;; Overridable, so a test can redirect the production handle to a temp file.
  (define visit-db-path (make-parameter (default-visit-db-path)))

  ;; Wall-clock unix seconds, behind a parameter so a seeded test can pin "now"
  ;; to a fixed epoch and make the ranking deterministic.
  (define visit-now
    (make-parameter (lambda () (time-second (current-time 'time-utc)))))

  ;; Keep the visit database to the person whose visits it is: the paths it
  ;; holds are the names of their projects.  The chmod goes BEFORE the first
  ;; PRAGMA, and the order is the point -- SQLite gives a -wal/-shm sidecar it
  ;; creates the mode of the database file as it finds it, so tightening the
  ;; database before anything writes means the sidecars are born owner-only too.
  ;; ":memory:" names no file, so it is left alone; and a chmod refused is not a
  ;; reason to lose the feature, so the failure is swallowed (posix-chmod raises
  ;; on failure).
  (define (restrict-to-owner! path)
    (unless (string=? path ":memory:")
      (guard (e [#t (void)])
        (posix-chmod path #o600))))

  ;; Open (or create) the visit database.  Returns a handle whose db may be #f
  ;; when libsqlite3 is unavailable or the open failed -- the handle is still
  ;; returned so every entry point can no-op through it, rather than the caller
  ;; having to test for #f itself.
  ;;
  ;; The order below is copied from (hafod editor history) because the order is
  ;; load-bearing: owner-only first (the sidecars inherit the mode), then the
  ;; busy timeout BEFORE the WAL conversion (the conversion rewrites the header
  ;; and wants the file exclusively, so it must be able to wait for a reader),
  ;; then WAL, then the table.
  (define open-visit-db
    (case-lambda
      [() (open-visit-db (visit-db-path))]
      [(path)
       (let ([db (sqlite3-open path)])
         (when db
           (restrict-to-owner! path)
           (sqlite3-exec db "PRAGMA busy_timeout=5000")
           (sqlite3-exec db "PRAGMA journal_mode=WAL")
           (sqlite3-exec db
             "CREATE TABLE IF NOT EXISTS visits (
                path TEXT PRIMARY KEY,
                rank REAL NOT NULL DEFAULT 0,
                last_access INTEGER NOT NULL DEFAULT 0)"))
         (let ([vdb (make-visitdb db #f)])
           ;; Close on the way out, and only when a database was actually
           ;; opened: a run that never opened one leaves the exit hooks as it
           ;; found them.  visit-db-close! clears the db field, so a hook that
           ;; somehow runs twice closes once.
           (when db
             (add-exit-hook! (lambda () (visit-db-close! vdb))))
           vdb))]))

  ;; Close the database, folding the write-ahead log back into the single file.
  ;; Switching the journal to DELETE checkpoints the -wal into the database and
  ;; unlinks it, so the user is left holding one self-contained file with nothing
  ;; legible lying beside it.  Best-effort: the pragma needs the database
  ;; exclusively and simply does not happen while another connection holds it,
  ;; and nothing here may keep us from closing -- this runs from an exit hook.
  (define (visit-db-close! vdb)
    (let ([db (visitdb-db vdb)])
      (when db
        (guard (e [#t #f])
          (sqlite3-exec db "PRAGMA journal_mode=DELETE"))
        (sqlite3-close db)
        (visitdb-db-set! vdb #f))))

  ;; The visit database would not take a write.  Say so once, then carry on:
  ;; frecency is an accessory to navigation, not navigation, and a cd that fell
  ;; over because the visit database was full would be worse than one that
  ;; quietly did not record the visit.  Once per session, on the console error
  ;; port, exactly as (hafod editor history) does for the same reason.
  (define (report-save-failure! vdb)
    (unless (visitdb-warned vdb)
      (visitdb-warned-set! vdb #t)
      (let ([port (console-error-port)])
        (display "hafod: directory-visit database could not be updated; frecency not recorded this session.\n"
                 port)
        (flush-output-port port))))

  ;; The zoxide frecency score: the stored rank scaled by a recency bucket, so a
  ;; directory visited within the last hour counts for four times its rank, and
  ;; one not seen in a fortnight for a quarter of it.  Pure and clock-injected --
  ;; the caller passes `now` -- so a seeded test is deterministic.  The bucket
  ;; boundaries are held as local second-constants.
  ;;
  ;; rank is a REAL in the table but always whole-valued: visit-record! starts it
  ;; at 1.0 and adds 1.0 a visit.  That is why it can be read back with
  ;; sqlite3-column-int64 without loss and no floating-point column accessor -- so
  ;; no new foreign procedure -- is needed.
  (define (frecency-score rank last-access now)
    (let* ([hour 3600]
           [day (* 24 hour)]
           [week (* 7 day)]
           [dt (max 0 (- now last-access))])
      (cond
        [(< dt hour) (* rank 4.0)]
        [(< dt day)  (* rank 2.0)]
        [(< dt week) (* rank 0.5)]
        [else        (* rank 0.25)])))

  ;; Record a visit to `path` at time `now`.  A no-op when there is no live
  ;; database.  Otherwise an UPSERT bound by ? parameters -- never string-built
  ;; from the path -- that sets rank 1.0 on the first visit and adds 1.0 on each
  ;; later one, stamping last_access either way.  The whole body is guarded so a
  ;; visit fault can never escape onto the navigation path; a step that did not
  ;; run to completion is reported once and swallowed.
  ;;
  ;; ON CONFLICT UPSERT needs libsqlite3 >= 3.24 (2018), which the runtime
  ;; library long since is.  An ancient library would instead want the two-
  ;; statement "UPDATE ...; if no row changed, INSERT ..." fallback.
  (define (visit-record! vdb path now)
    (let ([db (live-db vdb)])
      (when db
        (guard (e [#t (void)])
          (let ([stmt (sqlite3-prepare db
                        "INSERT INTO visits (path, rank, last_access) VALUES (?1, 1.0, ?2) ON CONFLICT(path) DO UPDATE SET rank = rank + 1.0, last_access = ?2")])
            (when stmt
              (sqlite3-bind-text stmt 1 path)
              (sqlite3-bind-int64 stmt 2 now)
              (let ([rc (sqlite3-step stmt)])
                (sqlite3-finalize stmt)
                (unless (= rc SQLITE_DONE)
                  (report-save-failure! vdb)))))))))

  ;; Smart case: an all-lower-case needle matches case-insensitively, a needle
  ;; carrying any upper-case character matches exactly.  A tiny local predicate,
  ;; rather than a dependency on the fuzzy matcher.
  (define (all-lower? s)
    (let ([len (string-length s)])
      (let loop ([i 0])
        (or (fx>= i len)
            (and (not (char-upper-case? (string-ref s i)))
                 (loop (fx+ i 1)))))))

  ;; #t when `needle` occurs anywhere in `path`, under the smart-case rule.  The
  ;; substring test is done here, in Scheme, over a STATIC SELECT -- the needle
  ;; never reaches the SQL -- so a needle shaped like SQL ("'; DROP TABLE ...")
  ;; is only ever compared as text: it can neither match a real path nor alter
  ;; the database.
  (define (path-matches? path needle)
    (if (all-lower? needle)
        (and (string-contains-ci path needle) #t)
        (and (string-contains path needle) #t)))

  ;; Fold every row of the visit table through (proc acc path rank last_access),
  ;; from `seed`.  The one place rows are read, so query-best and candidates
  ;; share the static SELECT and the column decoding.  Returns `seed` unchanged
  ;; when there is no live database or the statement will not prepare, and reads
  ;; rank with sqlite3-column-int64 (rank is whole-valued -- see frecency-score).
  (define (fold-visits vdb proc seed)
    (let ([db (live-db vdb)])
      (if (not db)
          seed
          (let ([stmt (sqlite3-prepare db
                        "SELECT path, rank, last_access FROM visits")])
            (if (not stmt)
                seed
                (let loop ([acc seed])
                  (if (= (sqlite3-step stmt) SQLITE_ROW)
                      (loop (proc acc
                                  (sqlite3-column-text stmt 0)
                                  (sqlite3-column-int64 stmt 1)
                                  (sqlite3-column-int64 stmt 2)))
                      (begin
                        (sqlite3-finalize stmt)
                        acc))))))))

  ;; The highest-frecency path CONTAINING `substr` (smart-case) for which the
  ;; caller's KEEP? predicate also holds, or #f.  Guarded so it never raises; #f
  ;; when there is no live database.
  ;;
  ;; KEEP? is how the jump skips a stale target.  This store is deliberately
  ;; filesystem-free -- it never itself asks whether a path still exists -- so
  ;; the shell layer passes file-directory? here and a top-ranked directory that
  ;; has since been DELETED is passed over for the next-best that is still on
  ;; disk, exactly as zoxide does, rather than the query committing to a dead top
  ;; match.  The three-argument form keeps every candidate -- the original
  ;; existence-free contract.  KEEP? is tried under its own guard, so a predicate
  ;; that raises on an odd path merely drops that path rather than the whole
  ;; search.
  (define visit-query-best
    (case-lambda
      [(vdb substr now) (visit-query-best vdb substr now (lambda (path) #t))]
      [(vdb substr now keep?)
       (guard (e [#t #f])
         (let ([best
                (fold-visits vdb
                  (lambda (best path rank last-access)
                    (if (and (path-matches? path substr)
                             (guard (e [#t #f]) (keep? path)))
                        (let ([score (frecency-score rank last-access now)])
                          (if (or (not best) (> score (cdr best)))
                              (cons path score)
                              best))
                        best))
                  #f)])
           (and best (car best))))]))

  ;; Every visited path, frecency-descending.  Guarded so it never raises; '()
  ;; when there is no live database.  list-sort is stable in Chez, so rows of
  ;; equal score keep their SELECT order.
  (define (visit-candidates vdb now)
    (guard (e [#t '()])
      (let ([scored
             (fold-visits vdb
               (lambda (acc path rank last-access)
                 (cons (cons path (frecency-score rank last-access now)) acc))
               '())])
        (map car
             (list-sort (lambda (a b) (> (cdr a) (cdr b)))
                        (reverse scored))))))

  ;; The lazily-opened production handle, gated on visit-recording?.
  ;;
  ;; visit-recording? is #f by default and is turned on at the interactive REPL
  ;; entry, so a non-interactive run (-c / -s / a batch script) that changes
  ;; directory never dlopens libsqlite3 nor creates the dotfile.  While it is #f,
  ;; ensure-visit-db! returns #f and opens nothing; once it is #t the database is
  ;; opened once, memoised, and reused.
  (define visit-recording? (make-parameter #f))
  (define %visit-db #f)
  (define (ensure-visit-db!)
    (and (visit-recording?)
         (or %visit-db
             (let ([vdb (open-visit-db (visit-db-path))])
               (set! %visit-db vdb)
               vdb))))

) ; end library
