;;; test/test-nav-frecency.ss -- Directory frecency ranks by the bucket, not the
;;; raw rank, and the substring query can never reach the SQL.
;;;
;;; The score is a stored rank scaled by a recency bucket: a directory seen in
;;; the last hour counts for four times its rank, one not seen in a fortnight for
;;; a quarter of it.  So a directory visited once an hour ago must outrank one
;;; visited five times a fortnight ago -- the whole point of frecency over a bare
;;; visit count.  With the clock pinned and rows seeded at known times straddling
;;; every bucket boundary, that ranked order is exact and deterministic.
;;;
;;; The query is the other half.  z <substr> filters the visited paths in Scheme
;;; over a STATIC SELECT -- the substring is never built into the SQL -- so a
;;; substring shaped like an injection ("'; DROP TABLE visits; --") is only ever
;;; compared as text: it matches nothing, and the table is still there afterwards.
;;;
;;; Entirely in-process -- one temporary database, no terminal.  Degrades to a
;;; printed note where libsqlite3 cannot be loaded.
;;;
;;; The library directories are pinned by ABSOLUTE path derived from the launch
;;; directory while it is still the repo root: a compiled library is located
;;; lazily at its first binding reference, and the portions added when the cd
;;; hook and the z builtin land will chdir, so a relative libdir could otherwise
;;; be resolved against a temporary directory.
;;; Copyright (c) 2026 Dominic Pearson.

(library-directories
  (let ([root (current-directory)])
    (list (cons (string-append root "/src") (string-append root "/src"))
          (cons root root))))
(import (test runner)
        (only (hafod shell visit-db)
              open-visit-db visit-record! visit-query-best visit-candidates
              visit-db-close! frecency-score visit-now visit-db-path
              visit-recording? ensure-visit-db!)
        (only (hafod shell builtins)
              run-builtin! builtin-cd-path nav-finder-proc)
        (only (hafod editor sqlite3)
              sqlite3-loaded? sqlite3-open sqlite3-close sqlite3-exec
              sqlite3-prepare sqlite3-bind-text sqlite3-bind-int64
              sqlite3-step sqlite3-finalize sqlite3-column-int64 SQLITE_ROW)
        (only (hafod posix) posix-mkstemp posix-close posix-unlink)
        (only (hafod process-state) chdir cwd)
        (chezscheme))

;; A fresh temporary database path (the mkstemp file itself, opened directly as
;; an empty database).
(define (temp-db-path label)
  (let-values ([(path fd) (posix-mkstemp
                            (string-append "/tmp/hafod-frecency-" label "-XXXXXX"))])
    (posix-close fd)
    path))

;; Remove a temporary database and any sidecar SQLite may have written beside it.
;; Each unlink is guarded: a sidecar that was never created is normal.
(define (remove-db! path)
  (for-each
    (lambda (suffix)
      (guard (e [#t #f]) (posix-unlink (string-append path suffix))))
    '("" "-journal" "-wal" "-shm")))

;; Seed a row on a connection of its own, with a known (path, rank, last_access).
;; The rank is bound as an integer -- production ranks are whole (rank starts at
;; 1.0 and increments by 1.0), so this is exactly the shape visit-record! writes
;; and the shape the query reads back with sqlite3-column-int64.  Direct binds,
;; because visit-record! would set rank and last_access itself rather than let a
;; test place them where the bucket boundaries need them.
(define (seed! db-path path rank last-access)
  (let* ([db (sqlite3-open db-path)]
         [stmt (and db (sqlite3-prepare db
                         "INSERT INTO visits (path, rank, last_access) VALUES (?1, ?2, ?3)"))])
    (when stmt
      (sqlite3-bind-text stmt 1 path)
      (sqlite3-bind-int64 stmt 2 rank)
      (sqlite3-bind-int64 stmt 3 last-access)
      (sqlite3-step stmt)
      (sqlite3-finalize stmt))
    (when db (sqlite3-close db))))

;; Read one path's stored rank on a connection of its own -- asserting what
;; reached the FILE, not what the shell's handle holds in memory.  Production
;; ranks are whole-valued (they start at 1.0 and step by 1.0), so column-int64
;; reads them back exactly: a single visit reads as 1.  Returns #f when the path
;; has no row, so a caller can test both presence and count.
(define (db-rank db-path path)
  (let* ([db (sqlite3-open db-path)]
         [stmt (and db (sqlite3-prepare db
                         "SELECT rank FROM visits WHERE path = ?1"))]
         [result
          (if stmt
              (begin
                (sqlite3-bind-text stmt 1 path)
                (let ([r (if (= (sqlite3-step stmt) SQLITE_ROW)
                             (sqlite3-column-int64 stmt 0)
                             #f)])
                  (sqlite3-finalize stmt)
                  r))
              #f)])
    (when db (sqlite3-close db))
    result))

(define (note . parts)
  (display "  note: ")
  (for-each display parts)
  (newline))

;; A fixed "now", and the bucket offsets around it.  Every offset is positive and
;; well clear of its boundary, so the ranking does not depend on the machine's
;; real clock and cannot straddle a boundary by a second.
(define NOW 1700000000)
(define half-hour   1800)      ; < 1h  -> x4
(define half-day    43200)     ; < 1d  -> x2
(define three-days  259200)    ; < 1wk -> x0.5
(define fortnight   1209600)   ; >=1wk -> x0.25

(test-begin "nav-frecency")

;; frecency-score is pure, so its exact per-bucket values need no database and
;; keep the suite non-vacuous even where libsqlite3 is absent.
(test-equal "a last-hour visit scores its rank times four"
  4.0 (frecency-score 1 (- NOW half-hour) NOW))
(test-equal "a last-day visit scores its rank times two"
  2.0 (frecency-score 1 (- NOW half-day) NOW))
(test-equal "a last-week visit scores its rank times a half"
  1.5 (frecency-score 3 (- NOW three-days) NOW))
(test-equal "an older visit scores its rank times a quarter"
  1.25 (frecency-score 5 (- NOW fortnight) NOW))

;; The heart of frecency: the bucket decides, not the raw rank.  A rank-1
;; directory seen in the last hour outscores a rank-5 one seen a fortnight ago.
(test-assert "a low-rank last-hour directory outranks a high-rank fortnight-old one"
  (> (frecency-score 1 (- NOW half-hour) NOW)
     (frecency-score 5 (- NOW fortnight) NOW)))

;; ----------------------------------------------------------------------
;; Seeded ranking and the injection-inert query.  Needs libsqlite3.
;; ----------------------------------------------------------------------

(define db-path (temp-db-path "seed"))
(visit-db-path db-path)                 ; redirect the production path to the temp file
(define probe (open-visit-db))          ; creates the table and loads libsqlite3

(if (not (sqlite3-loaded?))
    (begin
      (note "libsqlite3 is unavailable; skipping the seeded-ranking and injection assertions")
      (visit-db-close! probe)
      (remove-db! db-path))
    (begin
      ;; Close the probe so the table sits in a quiet single file, then seed rows
      ;; straddling every bucket boundary.  Ranks are chosen so the buckets --
      ;; not the ranks -- fix the order: the highest raw rank (5) is the lowest
      ;; score, the lowest (1) the highest.
      (visit-db-close! probe)
      (seed! db-path "/home/user/alpha-hot"  1 (- NOW half-hour))    ; x4   -> 4.0
      (seed! db-path "/home/user/alpha-day"  1 (- NOW half-day))     ; x2   -> 2.0
      (seed! db-path "/home/user/beta-week"  3 (- NOW three-days))   ; x0.5 -> 1.5
      (seed! db-path "/home/user/beta-old"   5 (- NOW fortnight))    ; x0.25-> 1.25

      (let ([vdb (open-visit-db)])
        (parameterize ([visit-now (lambda () NOW)])

          ;; The query picks the single highest-frecency path CONTAINING the
          ;; substring, out of the several that share it.
          (test-equal "z <substr> jumps to the highest-frecency path containing the substring"
            "/home/user/alpha-hot"
            (visit-query-best vdb "alpha" NOW))
          (test-equal "a different substring picks its own highest-frecency match"
            "/home/user/beta-week"
            (visit-query-best vdb "beta" NOW))

          ;; The full candidate list, frecency-descending, asserted exactly.
          (test-equal "the picker's candidates come back frecency-descending"
            (list "/home/user/alpha-hot" "/home/user/alpha-day"
                  "/home/user/beta-week" "/home/user/beta-old")
            (visit-candidates vdb NOW))

          ;; Injection-inert: a SQL-shaped substring matches nothing (no seeded
          ;; path contains that literal) ...
          (test-equal "a SQL-shaped substring matches no path"
            #f
            (visit-query-best vdb "'; DROP TABLE visits; --" NOW))

          ;; ... and the table is still whole afterwards: the substring reached a
          ;; Scheme-side filter over a static SELECT, never the SQL, so it could
          ;; not have dropped anything.
          (test-equal "and the visits table is intact -- the substring never reached the SQL"
            (list "/home/user/alpha-hot" "/home/user/alpha-day"
                  "/home/user/beta-week" "/home/user/beta-old")
            (visit-candidates vdb NOW)))

        (visit-db-close! vdb))

      (remove-db! db-path)))

;; ----------------------------------------------------------------------
;; The cd hook and the z builtins wired into the shell: one visit per change,
;; the injection-safe jump, the injected picker, and fail-quiet cd.  Needs
;; libsqlite3 and the shell dispatch.  Everything filesystem-touching happens
;; inside a temporary parent and is unwound on exit -- cwd restored, every
;; artefact and database removed, even on failure -- exactly as the auto-cd
;; suite does.
;; ----------------------------------------------------------------------

(if (not (sqlite3-loaded?))
    (note "libsqlite3 is unavailable; skipping the cd-hook and z-jump proofs")
    (let* ([orig (cwd)]
           [P "/tmp/zzhafod-frecency-nav"]
           [recorded (string-append P "/recorded")]
           [picked   (string-append P "/picked")]
           [offdir   (string-append P "/off-target")]
           [faultdir (string-append P "/fault-target")]
           ;; A directory whose LITERAL name carries a space and a `;` -- the
           ;; shape a naive "cd <name>" rebuild-and-reparse would split and run.
           [evil     (string-append P "/; touch PWNED")]
           [pwned    (string-append P "/PWNED")]
           ;; A substring-matching pair for the stale-target jump: a
           ;; higher-frecency directory that is never created on disk, and a
           ;; lower-frecency one that is.  z <substr> must skip the missing top
           ;; match and land on the lower-ranked directory that still exists.
           [zgone    (string-append P "/zap-gone")]        ; higher rank, absent
           [zhere    (string-append P "/zap-here")]        ; lower rank, present
           [navdb    (string-append P "/nav-visits.db")]   ; the shared recording DB
           [freshdb  (string-append P "/off-visits.db")])  ; must never be created
      (define (rmdir-if d) (when (file-exists? d) (delete-directory d)))
      (define (rmfile-if f) (when (file-exists? f) (delete-file f)))
      (define (cleanup!)
        (chdir orig)
        (remove-db! navdb)
        (remove-db! freshdb)
        (rmfile-if pwned)
        (rmdir-if recorded)
        (rmdir-if picked)
        (rmdir-if offdir)
        (rmdir-if faultdir)
        (rmdir-if evil)
        (rmdir-if zhere)
        (rmdir-if zgone)
        (rmdir-if P))
      (cleanup!)                 ; clear any leftover from a prior run
      (dynamic-wind
        (lambda () #t)
        (lambda ()
          (mkdir P)
          (mkdir recorded)
          (mkdir picked)
          (mkdir offdir)
          (mkdir faultdir)
          (mkdir evil)

          ;; --- fail-quiet cd with recording off: no database file appears ---
          ;; ensure-visit-db! is gated on visit-recording?, so with it off a cd
          ;; never opens (nor dlopens for) the database: cwd moves and freshdb is
          ;; never created.  Run first, while the production handle is still
          ;; unopened.
          (parameterize ([visit-recording? #f]
                         [visit-db-path freshdb])
            (chdir P)
            (run-builtin! (string-append "cd " offdir))
            (test-equal "cd with recording off still changes directory"
              offdir (cwd))
            (test-assert "cd with recording off creates no visit-database file"
              (not (file-exists? freshdb))))

          ;; --- recording on: one change, one visit; pushd AND popd each record ---
          (parameterize ([visit-recording? #t]
                         [visit-db-path navdb]
                         [visit-now (lambda () NOW)])

            ;; A single pushd records the target exactly once.  cd-to! is the one
            ;; recording seam, so pushd's own bookkeeping does not double-count the
            ;; visit: recorded comes back rank 1 (a single visit), never 2.
            (chdir P)                    ; the directory popd will return to
            (run-builtin! (string-append "pushd " recorded))
            (test-equal "pushd changed into the target"
              recorded (cwd))
            (test-equal "one pushd records the target exactly once (rank 1, not 2)"
              1 (db-rank navdb recorded))

            ;; popd routes through the same seam, so its landing is recorded too.
            (run-builtin! "popd")
            (test-equal "popd returned to the start"
              P (cwd))
            (test-assert "popd also records its landing directory"
              (db-rank navdb P))

            ;; The headline: z <substr> enters a metacharacter-named directory
            ;; LITERALLY.  Record the evil directory (through the same cd seam),
            ;; step away, then jump by a safe substring of its name.  A verbatim
            ;; chdir cannot split on the space or the `;`; a rebuilt "cd <name>"
            ;; would -- so cwd lands exactly on it AND no PWNED file appears.
            (builtin-cd-path evil)
            (test-equal "recording the metacharacter-named directory landed on it"
              evil (cwd))
            (chdir P)
            (run-builtin! "z touch")
            (test-equal "z <substr> jumps into the metacharacter-named directory literally"
              evil (cwd))
            (test-assert "the metacharacter name created no side-effect file"
              (not (file-exists? pwned)))

            ;; The bare z / zi picker: an injected finder chooses a candidate and
            ;; the jump lands through the same literal chdir.  A finder returning a
            ;; path jumps there; one that cancels (#f) leaves cwd where it was.
            (chdir P)
            (parameterize ([nav-finder-proc (lambda (items prompt) picked)])
              (run-builtin! "z")
              (test-equal "a stub finder drives bare z to the chosen directory"
                picked (cwd)))
            (chdir P)
            (parameterize ([nav-finder-proc (lambda (items prompt) #f)])
              (run-builtin! "z")
              (test-equal "a finder that cancels leaves the directory unchanged"
                P (cwd)))

            ;; z <substr> skips a higher-frecency directory that has since been
            ;; deleted and falls through to the next-best match that still
            ;; exists -- zoxide's defining behaviour.  Two matches share the
            ;; substring "zap": the top-ranked one is never created on disk, the
            ;; lower-ranked one is.  The existence-free query still names the
            ;; missing top match (so its higher rank is real, not an artefact of
            ;; the test), but the jump must not error on that dead path -- it
            ;; lands on the lower-ranked directory that is actually there.
            (mkdir zhere)
            (seed! navdb zgone 5 (- NOW half-hour))   ; 5 x4 = 20.0, but absent
            (seed! navdb zhere 1 (- NOW half-hour))   ; 1 x4 =  4.0, present
            (test-equal "the existence-free query still names the higher-ranked (missing) match"
              zgone (visit-query-best (ensure-visit-db!) "zap" NOW))
            (chdir P)
            (run-builtin! "z zap")
            (test-equal "z <substr> skips the deleted top match and lands on the next that exists"
              zhere (cwd)))

          ;; --- fail-quiet cd when the database faults under the live handle ---
          ;; Drop the visits table on a second connection, out from under the
          ;; memoised recording handle, then cd: the record attempt meets a table
          ;; that is gone, is swallowed by cd-to!'s guard, and the directory change
          ;; still happens.  (A per-cd unwritable path cannot be exercised through
          ;; the production handle in one process -- it is opened and memoised once
          ;; -- so the fault is induced on the live handle, proving the same "a
          ;; visit fault never breaks cd" invariant.)
          (let ([c (sqlite3-open navdb)])
            (when c
              (sqlite3-exec c "DROP TABLE IF EXISTS visits")
              (sqlite3-close c)))
          (parameterize ([visit-recording? #t]
                         [visit-db-path navdb]
                         [visit-now (lambda () NOW)])
            (chdir P)
            (run-builtin! (string-append "cd " faultdir))
            (test-equal "cd still changes directory when the visit write faults"
              faultdir (cwd))))
        (lambda () (cleanup!)))))

(test-end)
