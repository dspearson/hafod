;;; test/test-history-concurrency.ss -- Two hafod instances sharing one history
;;; database must not lose to one another: a session that meets a lock somebody
;;; else is holding has to wait it out, not drop the work it was asked to do.
;;;
;;; Two things can be waiting on that lock, and both are proved here against real
;;; processes.
;;;
;;;   * The WRITE.  A database opened with no busy_timeout gives a contended writer
;;;     no time at all -- SQLite reports SQLITE_BUSY the moment it finds the write
;;;     lock taken.  history-add! never looks at what its step returned, so the
;;;     insert is simply dropped: nothing raises, nothing is printed, and the line
;;;     the user typed is just not in their history the next time they look for it.
;;;
;;;   * The UPGRADE.  A history file written before any of this existed is on
;;;     SQLite's default rollback journal, and carrying it over to WAL rewrites the
;;;     database header -- which wants an EXCLUSIVE lock, and so must wait for a
;;;     reader to finish.  The busy_timeout governs that wait, but only if it has
;;;     been set BEFORE the conversion is asked for: with the PRAGMAs the other way
;;;     round the conversion runs with the default timeout of zero, comes straight
;;;     back SQLITE_BUSY, and -- its return discarded like every other -- the session
;;;     runs its whole life on the rollback journal believing it has WAL.  Nothing
;;;     says so.  The next session converts it, or does not, by the same coin toss.
;;;
;;; WHY EACH SIDE IS A PROCESS OF ITS OWN, AND NOT A FORK OF THIS ONE.
;;;
;;; What is claimed here is a fact about two hafod INSTANCES.  Two children forked
;;; from this suite are not two instances: they are one process's address space,
;;; twice, sharing every library it had already loaded and every lock those
;;; libraries held at the moment of the fork.  So the suite re-execs ITSELF -- the
;;; same script, given a role and a database path -- and the children are genuinely
;;; separate programs, started from nothing, meeting only at the database.  That is
;;; the thing the claim is about, and now it is the thing being run.
;;;
;;; It is also the difference between a suite that passes and one that passes half
;;; the time.  A forked child that then calls into libsqlite3 without an exec is
;;; undefined on Darwin -- after fork() a child may call only async-signal-safe
;;; functions until it execs, and libsqlite3 sits on malloc and on the platform's
;;; own runtime, any of which may have held a lock at the instant the fork was
;;; taken.  Whether the child then wedges or runs is down to what the parent
;;; happened to be doing, which is why this suite used to fail on macOS in about
;;; one run of two -- the holder raising, and the writer, quite correctly, timing
;;; out waiting for a lock nobody had taken.  Linux is more forgiving and hid it.
;;; An exec'd child has none of the problem: it is a fresh image, and libsqlite3 in
;;; it is a library like any other.  Nothing here is fork-only-safe any more.
;;;
;;; THE RENDEZVOUS.  Both proofs are an appointment between two real processes,
;;; kept with files and the wall clock, not a hammering of one and a hope:
;;;
;;;   * The one that MEETS the lock starts first in the appointment, not the one
;;;     that holds it: it drops a READY file the moment it is up, and only then
;;;     begins waiting.  So the held window opens after its peer is already
;;;     watching for it, and a process that took a third of a second to start
;;;     cannot arrive to find the window shut.  Rendezvous by handshake, never by
;;;     hoping two processes start at the same speed -- and they do not: an exec'd
;;;     process pays a start-up an in-process fork does not.
;;;   * The HOLDER waits for that READY file, opens a connection of its own, and
;;;     takes the lock deterministically.
;;;   * A LOCK file is the signal that the lock is genuinely held.  It is dropped
;;;     ONLY once the open returned a handle AND the statement that takes the lock
;;;     returned SQLITE_OK -- the return code is checked, never discarded.  This is
;;;     the load-bearing part of the whole design.  A holder that quietly failed to
;;;     take the lock and dropped its file anyway would leave the other process
;;;     working against an unlocked database; everything would succeed; and the
;;;     case would pass on a tree that still throws the work away.  Gating the file
;;;     on a real SQLITE_OK turns a holder failure into the other process's wait
;;;     timing out -- a loud failure -- instead of a quiet false pass.
;;;   * The lock file is removed BEFORE the lock is released, never after, so that
;;;     "the file is there" implies "the lock is held" at every instant there is.
;;;     The other way round leaves a window in which the file still says held and
;;;     the lock is already free -- and a process that read it in that window would
;;;     work against nothing and pass, which is the one failure this design exists
;;;     to make impossible.
;;;   * The process that meets the lock waits on the WALL CLOCK (poll-until -- never
;;;     a count of attempts, which bounds the number of tries and not the time they
;;;     are given) for that file, and checks it once more immediately before the
;;;     operation that has to meet the lock.  A file that never arrives, or that has
;;;     gone by the time it matters, means the appointment was not kept: it exits
;;;     non-zero rather than work against a lock nobody holds.
;;;   * The parent reaps both, insists BOTH left cleanly, and only then reads the
;;;     file back on a connection of its own.
;;;
;;; The write rendezvous runs on a file whose journal mode is settled BEFORE the
;;; children exist -- open-history on the fresh file, one seed row, closed again --
;;; so the writer's own journal_mode PRAGMA is a WAL-to-WAL no-op and the case is
;;; about the insert and nothing else.  Without the busy_timeout that insert meets
;;; the held lock, comes back SQLITE_BUSY, is dropped on the floor, and its row is
;;; not on the file (count 0).  With it, the writer waits out the second the holder
;;; sits on the lock and its row is there (count 1).  The holder's own row is
;;; asserted too: that is what proves the lock was really taken and really committed.
;;;
;;; The upgrade rendezvous runs on the file that case sidesteps: a rollback-journal
;;; history, built here exactly as an older hafod left it, with a READER holding it.
;;; A read lock is what the conversion has to outlast, and is the one lock the busy
;;; handler will genuinely wait through -- SQLite declines to wait for a RESERVED
;;; lock, deadlock being the likelier reading of it, but waits for readers to clear
;;; before it takes the EXCLUSIVE lock it needs.  So the reader releases while the
;;; session is still inside the PRAGMA, and the question is only whether the session
;;; was given any time in which to notice.  With the timeout set first it waits the
;;; reader out and the file comes back "wal"; with the conversion asked for first it
;;; is refused instantly and the file is still "delete" -- rollback journal, for the
;;; life of that session, silently.
;;;
;;; A watchdog child SIGKILLs the suite after thirty seconds, so a wait that never
;;; ends fails the run deterministically instead of hanging it.  It is the one child
;;; still forked rather than exec'd, and legitimately so: it sleeps, signals, and
;;; leaves, and every call it makes between the fork and its exit is a bare syscall.
;;;
;;; Real processes, temporary files, no terminal -- so it runs the same way
;;; everywhere.  Degrades to a printed note where libsqlite3 cannot be loaded.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (test poll)
        (hafod editor history)
        (only (hafod editor sqlite3)
              sqlite3-open sqlite3-close sqlite3-exec sqlite3-prepare
              sqlite3-step sqlite3-finalize sqlite3-bind-text
              sqlite3-column-text sqlite3-column-int64 sqlite3-loaded?
              SQLITE_OK SQLITE_ROW SQLITE_DONE)
        (only (hafod posix)
              posix-fork posix-exec posix-waitpid posix-_exit posix-getpid
              posix-kill posix-sleep posix-mkstemp posix-close posix-unlink
              status:exit-val)
        (only (hafod process) exec-path-search exec-path-list)
        (only (hafod signal) SIGKILL)
        (chezscheme))

;; ======================================================================
;; Everything above the role dispatch is shared by the suite and by every
;; process it starts -- they are the same file.  It must therefore be free of
;; side effects: a define here runs in five processes, not one.
;; ======================================================================

;; How long a process will wait for its peer to keep the appointment.  Generous:
;; it is only ever paid in full when the peer never came at all (a failure, and
;; the run says so), whereas the moment it is too short is the moment a slow
;; start-up on a loaded machine turns into a red run that means nothing.
(define rendezvous-ms 10000)

;; How long each lock is held once taken.  Long enough that the process meeting it
;; -- already up, already watching for the file that says the lock is taken -- is
;; certainly inside its operation while the lock is still held.
(define write-hold-seconds 1)
(define read-hold-seconds 2)

(define seed-row   "(display \"written before the others started\")")
(define holder-row "(display \"written by the process holding the lock\")")
(define writer-row "(display \"written by the process that met the held lock\")")
(define legacy-row "(display \"typed into a hafod that had never heard of WAL\")")

;; The three files of the appointment, all named off the database path -- which
;; mkstemp made unpredictable, so they are not paths anything else on the host
;; could have got to first, and a child needs nothing but its role and that one
;; path to find every one of them.
(define (ready-marker path)   (string-append path ".ready"))
(define (lock-marker path)    (string-append path ".locked"))
(define (live-mode-file path) (string-append path ".mode"))

(define (touch! path) (close-port (open-output-file path 'replace)))

;; Removing a file that is not there is the normal case here, not an error.
(define (drop! path) (guard (e [#t #f]) (posix-unlink path)))

;; Wait, on the wall clock, for PATH to appear.  #t if it did, #f on timeout.
(define (wait-for-file path)
  (poll-until rendezvous-ms
              (lambda () (posix-getpid))
              (lambda () (file-exists? path))))

;; A fresh temporary database path.  mkstemp leaves a zero-length file behind,
;; which SQLite opens as an empty database.
(define (temp-db-path label)
  (let-values ([(path fd) (posix-mkstemp
                            (string-append "/tmp/hafod-history-" label "-XXXXXX"))])
    (posix-close fd)
    path))

;; Remove a temporary database, the files of its appointment, and any sidecar
;; SQLite may have written beside it -- a WAL database keeps a -wal and a -shm next
;; to it while a connection is open -- so the suite leaves nothing behind.
(define (remove-db! path)
  (for-each (lambda (suffix) (drop! (string-append path suffix)))
            '("" "-journal" "-wal" "-shm" ".ready" ".locked" ".mode")))

;; Insert one row on a raw connection, reporting whether it actually persisted.
;; The return of the step is READ here -- an insert that came back SQLITE_BUSY
;; wrote nothing, and saying otherwise is the very defect under test.
(define (insert-row! db input)
  (let ([stmt (sqlite3-prepare db
                "INSERT INTO history (input, mode) VALUES (?1, ?2)")])
    (and stmt
         (begin
           (sqlite3-bind-text stmt 1 input)
           (sqlite3-bind-text stmt 2 "scheme")
           (let ([rc (sqlite3-step stmt)])
             (sqlite3-finalize stmt)
             (= rc SQLITE_DONE))))))

;; How many rows on the FILE carry exactly this input, counted on a connection of
;; its own -- no child's -- so what is asserted is what reached the file and not
;; what some handle was holding in memory.  A query that could not be run at all
;; comes back -1, which fails an assertion loudly rather than reading as a quiet
;; "no rows".
(define (rows-with-input path input)
  (let* ([db (sqlite3-open path)]
         [stmt (and db (sqlite3-prepare db
                         "SELECT COUNT(*) FROM history WHERE input = ?1"))]
         [n (if (and stmt
                     (begin (sqlite3-bind-text stmt 1 input)
                            (= (sqlite3-step stmt) SQLITE_ROW)))
                (sqlite3-column-int64 stmt 0)
                -1)])
    (when stmt (sqlite3-finalize stmt))
    (when db (sqlite3-close db))
    n))

;; The journal mode recorded in the FILE, read on a connection of its own.  SQLite
;; keeps it in the database header -- which is what makes a WAL upgrade durable, and
;; what makes it readable back here once every handle has been closed.
(define (journal-mode-of path)
  (let* ([db (sqlite3-open path)]
         [stmt (and db (sqlite3-prepare db "PRAGMA journal_mode"))]
         [mode (if (and stmt (= (sqlite3-step stmt) SQLITE_ROW))
                   (sqlite3-column-text stmt 0)
                   #f)])
    (when stmt (sqlite3-finalize stmt))
    (when db (sqlite3-close db))
    mode))

;; The journal mode a SESSION was actually running in, carried back to the parent
;; that asserts on it.
;;
;; It has to be observed from inside the session, while it is live: what the session
;; leaves behind cannot answer the question any more.  Closing checkpoints the log
;; back into the database and returns the file to the rollback journal (see
;; history-close!), so the settled file reads "delete" whether the session ran in WAL
;; or never converted at all -- the correct run and the bug leave the same thing
;; lying there.  A child process returns nothing but an exit code, and the exit code
;; is already saying whether the appointment was kept, so the mode is written beside
;; the database and read back from there.
(define (record-live-mode! path mode)
  (when mode
    (let ([p (open-output-file (live-mode-file path) 'replace)])
      (display mode p)
      (close-port p))))

(define (recorded-live-mode path)
  (let ([f (live-mode-file path)])
    (and (file-exists? f)
         (let* ([p (open-input-file f)]
                [s (get-string-all p)])
           (close-port p)
           (if (eof-object? s) "" s)))))

;; Build the database an earlier hafod left behind: the schema it created, with one
;; row in it, and not one journal_mode PRAGMA anywhere near it -- so the file keeps
;; SQLite's default rollback journal, exactly as a real history.db written before any
;; of this did.  open-history is deliberately not used: it is the thing that converts
;; a file to WAL, and this file has to reach the session unconverted.  Every return is
;; checked, so a fixture that did not get built says so rather than being mistaken for
;; a passing readback.
(define (build-rollback-history! path row)
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
                      (insert-row! db row))])
           (sqlite3-close db)
           built))))

(define (note . parts)
  (display "  note: ")
  (for-each display parts)
  (newline))

;; ======================================================================
;; The four roles.  Each is a whole process: it is handed a database path, keeps
;; its half of the appointment, and leaves by an exit code.
;;
;; The codes are shared, so that a failing run says which half broke and how:
;;
;;   0  it did what it was asked
;;   1  it could not take the lock at all, or something raised
;;   2  it held the lock but the work inside it would not commit
;;   3  the appointment was never kept: a file that had to arrive never did
;;   4  the lock was already gone by the moment it mattered, so there was nothing
;;      to meet -- refused, rather than work against an unlocked database and pass
;; ======================================================================

;; Takes the WRITE lock and sits on it while its peer runs into it.
(define (hold-the-write-lock! path)
  (let ([db (sqlite3-open path)])
    (cond
      [(not db) 1]
      ;; The peer must be up and watching before the window opens; see the header.
      [(not (wait-for-file (ready-marker path)))
       (sqlite3-close db)
       3]
      [(not (= SQLITE_OK (sqlite3-exec db "BEGIN IMMEDIATE")))
       (sqlite3-close db)
       1]
      [(not (insert-row! db holder-row))
       (sqlite3-exec db "ROLLBACK")
       (sqlite3-close db)
       2]
      [else
       ;; The lock is held.  Now, and only now, the writer may go.
       (touch! (lock-marker path))
       (posix-sleep write-hold-seconds)
       ;; Stop saying the lock is held BEFORE letting go of it, never after.
       (drop! (lock-marker path))
       (let ([rc (sqlite3-exec db "COMMIT")])
         (sqlite3-close db)
         (if (= rc SQLITE_OK) 0 2))])))

;; Meets that write lock, and does with the history database exactly what hafod
;; does: opens it through open-history and adds an entry.
;;
;; history-add! swallows the step's return either way -- what is asked here is not
;; what it reported but whether the row reached the file, which the parent reads
;; back once both processes have gone.
(define (write-against-the-held-lock! path)
  (touch! (ready-marker path))
  (if (not (wait-for-file (lock-marker path)))
      3
      (let ([h (open-history path)])
        ;; The lock has to be held at the moment the insert is OFFERED -- that is
        ;; the operation whose fate is being asserted on.  Checked here, between
        ;; the open and the insert, rather than earlier: an open that had somehow
        ;; sat out the whole window would leave an insert meeting nothing, and a
        ;; row would land whatever the busy_timeout said.
        (if (not (file-exists? (lock-marker path)))
            (begin (history-close! h) 4)
            (begin
              (history-add! h writer-row)
              (history-close! h)
              0)))))

;; Holds a READ lock on the legacy file: a transaction that has read holds a SHARED
;; lock until it ends, and a SHARED lock is exactly what the WAL conversion's
;; EXCLUSIVE lock must wait for.  It is the lock to hold here, and not a write lock:
;; SQLite declines to wait for a RESERVED lock at all (a deadlock is the likelier
;; reading of that contention) and would refuse the conversion instantly however long
;; its timeout, proving nothing about the timeout.
;;
;; The lock is taken at the first READ, not at BEGIN -- a deferred transaction locks
;; nothing until it touches the file -- so the SELECT is what makes it real, and its
;; return code is checked before the lock file is dropped.
(define (hold-the-read-lock! path)
  (let ([db (sqlite3-open path)])
    (cond
      [(not db) 1]
      [(not (wait-for-file (ready-marker path)))
       (sqlite3-close db)
       3]
      [(not (= SQLITE_OK (sqlite3-exec db "BEGIN")))
       (sqlite3-close db)
       1]
      [(not (= SQLITE_OK (sqlite3-exec db "SELECT COUNT(*) FROM history")))
       (sqlite3-exec db "ROLLBACK")
       (sqlite3-close db)
       1]
      [else
       (touch! (lock-marker path))
       (posix-sleep read-hold-seconds)
       (drop! (lock-marker path))
       (let ([rc (sqlite3-exec db "COMMIT")])
         (sqlite3-close db)
         (if (= rc SQLITE_OK) 0 2))])))

;; Meets that read lock, and does with the history file it finds exactly what a hafod
;; session does: opens it through open-history.  Whether the conversion survived the
;; contention is not asked of the session -- open-history discards what its PRAGMA
;; returned, which is the whole trouble -- but of the running session itself, which
;; records the mode it is in for the parent to assert on.
;;
;; Here the lock must be held at the moment of the OPEN: open-history is the operation
;; under test, and its PRAGMA is what has to meet the reader.  A session that arrived
;; to find the lock already released would convert an uncontended file and pass on a
;; tree that would have been refused, so it exits 4 and fails the run instead.
(define (open-against-the-held-read-lock! path)
  (touch! (ready-marker path))
  (cond
    [(not (wait-for-file (lock-marker path))) 3]
    [(not (file-exists? (lock-marker path))) 4]
    [else
     (let ([h (open-history path)])
       ;; Take the reading the whole appointment exists for while the session is
       ;; still standing: the mode it is actually running in, not the mode of the
       ;; file it leaves.  See record-live-mode!.
       (record-live-mode! path (journal-mode-of path))
       (history-close! h)
       0)]))

;; ======================================================================
;; The role dispatch.
;;
;; This file is BOTH the suite and the programme each of its processes runs: the
;; suite below re-execs this very script with a role and a database path, and a
;; process that arrives here holding a role is one of those.  It does its part and
;; leaves by posix-_exit, so nothing past this point -- not one assertion, not one
;; temporary file -- is ever reached by a child.
;;
;; Which is why the dispatch sits ABOVE the first side effect in the file: the
;; suite's temporary databases are made by mkstemp at the top level, and a child
;; that ran those forms would strew a database of its own across /tmp on every
;; start.  Everything above this line is a definition and nothing more.
;;
;; A raise inside a role is exit 1, not a Chez backtrace on stderr and an exit code
;; nobody can read.
;; ======================================================================

(define (run-role! role path)
  (posix-_exit
    (guard (e [#t 1])
      (cond
        [(string=? role "hold-the-write-lock")  (hold-the-write-lock! path)]
        [(string=? role "write-against-it")     (write-against-the-held-lock! path)]
        [(string=? role "hold-the-read-lock")   (hold-the-read-lock! path)]
        [(string=? role "open-against-it")      (open-against-the-held-read-lock! path)]
        [else 1]))))

(let ([args (command-line-arguments)])
  (unless (null? args)
    (if (= (length args) 2)
        (run-role! (car args) (cadr args))
        (posix-_exit 1))))

;; ======================================================================
;; From here down: the suite.  Only the process make started ever gets this far.
;; ======================================================================

;; The interpreter the children run under.  $SCHEME is the one make or just
;; actually selected and exported into this suite's environment; prefer it, so the
;; children are the build's own Chez rather than whichever interpreter a
;; manipulated PATH happens to offer first -- a mismatched one raises an
;; incompatible-fasl error that would surface here as a spurious failure.
;; exec-path-search takes a name containing a slash as-is (checking it is
;; executable) and searches PATH for a bare one, so the single call covers both
;; forms $SCHEME may hold.  The scheme/chez-scheme fallback mirrors the Makefile's
;; own auto-detection and keeps this suite runnable when invoked by hand.
(define scheme-bin
  (let loop ([candidates (let ([named (getenv "SCHEME")])
                           (if (and named (not (string=? named "")))
                               (list named "scheme" "chez-scheme")
                               (list "scheme" "chez-scheme")))])
    (cond [(null? candidates) #f]
          [(exec-path-search (car candidates) (exec-path-list))]
          [else (loop (cdr candidates))])))

;; This script, named as it was invoked, so a child can be told to run it.  The
;; children inherit this process's working directory, so a relative path resolves
;; in the child exactly as it did here -- and --libdirs .:src, which is relative to
;; that same directory, resolves with it.
(define suite-path (car (command-line)))

;; Start one role as a process of its own.
;;
;; The fork is followed IMMEDIATELY by the exec: between the two the child does
;; nothing but hand libc an argv, which is the only thing a forked child is allowed
;; to assume it can do.  All the work -- the connection, the lock, the insert -- is
;; done by the exec'd image, which is a fresh process with a runtime of its own and
;; no inherited locks to deadlock on.  See the header: doing that work in the forked
;; child instead is what made this suite a coin toss on macOS.
;;
;; An exec that fails cannot report anything up here, so it leaves 127 behind: a
;; non-zero the suite fails on rather than a silent absence.
(define (spawn-role role path)
  ;; The child's exec drops this process's pending output with the rest of the
  ;; image it replaces, so nothing can be printed twice; flushed all the same, so
  ;; the run's output arrives in the order it was written.
  (flush-output-port (current-output-port))
  (let ([pid (posix-fork)])
    (if (zero? pid)
        (begin
          (guard (e [#t #f])
            (posix-exec scheme-bin
                        (list scheme-bin "--libdirs" ".:src"
                              "--script" suite-path role path)))
          (posix-_exit 127))
        pid)))

;; Reap one child, as an exit code.  #f is a child that did not leave by exiting at
;; all -- a signal, say -- and fails the assertion just as loudly as a bad code.
(define (reap pid)
  (let-values ([(p status) (posix-waitpid pid 0)])
    (status:exit-val status)))

;; The appointment: start both processes, let them keep it, and reap them both.
;; Returns their exit codes as (holder other).
(define (rendezvous hold-role meet-role path)
  (let* ([holder (spawn-role hold-role path)]
         [other (spawn-role meet-role path)])
    (list (reap holder) (reap other))))

;; Fork a sibling that SIGKILLs this process after SECONDS, run THUNK, then cancel
;; the sibling on the happy path and return the thunk's value.  A wait that never
;; ends -- a writer blocked for ever on a lock nobody releases -- dies here rather
;; than hanging the suite out to the per-suite backstop.
;;
;; The only child this suite still forks without exec'ing, and the one place that is
;; sound: between the fork and its exit it sleeps, signals, and leaves, and every
;; call it makes is a bare syscall.  It touches no library that could have been
;; holding a lock when the fork was taken, which is the whole of the hazard the
;; roles were exec'd to escape.
;;
;; victim is captured in an OUTER let, fully bound before the inner (posix-fork):
;; Chez evaluates let inits right-to-left, so a single flat let would evaluate the
;; fork before (posix-getpid) and the watchdog child would capture its own pid and
;; SIGKILL itself, never the test process.  The nested lets fix the order.
(define (with-watchdog seconds thunk)
  (let ([victim (posix-getpid)])
    (flush-output-port (current-output-port))
    (let ([wpid (posix-fork)])
      (if (zero? wpid)
          (begin (posix-sleep seconds) (posix-kill victim SIGKILL) (posix-_exit 0))
          (let ([result (thunk)])
            (posix-kill wpid SIGKILL)
            (posix-waitpid wpid 0)
            result)))))

(test-begin "history-concurrency")

;; Nothing below can be run at all without an interpreter to run the other
;; processes under, and a suite that could not start them would otherwise fail
;; further down as a puzzle about exit codes rather than as the plain fact it is.
(test-assert "an interpreter was found to run the other processes under"
  (and scheme-bin #t))

(define db-path (temp-db-path "concurrency"))
(define legacy-path (temp-db-path "legacy-under-lock"))

;; The schema and the journal mode of the write rendezvous's file are both settled
;; here, before any process is started: this is what records WAL in that file's
;; header, so the writer's own journal_mode PRAGMA is a WAL-to-WAL no-op rather than
;; a conversion of its own -- which is the other case entirely, and has a rendezvous
;; of its own below.  It is also the call that loads libsqlite3, so sqlite3-loaded?
;; can be trusted from here on.
(define seed (open-history db-path))

(if (not (sqlite3-loaded?))
    (begin
      (note "libsqlite3 is unavailable; skipping the contended-session assertions")
      (history-close! seed)
      (remove-db! db-path)
      (remove-db! legacy-path))
    (begin
      (history-add! seed seed-row)
      (history-close! seed)

      ;; ---- the write --------------------------------------------------------

      (let ([exits (with-watchdog 30
                     (lambda ()
                       (rendezvous "hold-the-write-lock" "write-against-it"
                                   db-path)))])

        ;; The appointment was kept.  Without this the row counts below could not be
        ;; read for what they say: a holder that never took the lock, or a writer
        ;; that gave up waiting for it -- or arrived to find it already released --
        ;; would leave a count that means something else entirely.  Fail on the
        ;; exits, loudly, before judging anything by a row.
        (test-equal "both writers ran the rendezvous through to a clean exit"
          '(0 0)
          exits)

        ;; The observation.  The writer met a write lock somebody else was holding.
        ;; With a busy_timeout it waits the second out and its line is on the file;
        ;; without one its insert comes back BUSY, is discarded unread, and the line
        ;; is gone.
        (test-equal "a writer that meets a held write lock still lands its row"
          1
          (rows-with-input db-path writer-row))

        ;; The control.  The lock was not merely thought to be held: the process
        ;; holding it wrote a row inside that very transaction and committed it, and
        ;; here it is.  Two processes, one database, both rows.
        (test-equal "the writer that held the lock landed its own row"
          1
          (rows-with-input db-path holder-row)))

      ;; ---- the upgrade ------------------------------------------------------

      (let ([built? (build-rollback-history! legacy-path legacy-row)])

        ;; The fixture is really on disk.  The rendezvous below reads this file, so a
        ;; fixture that quietly failed to build would turn every assertion after it
        ;; into a question about nothing.
        (test-assert "the file an earlier hafod would have left is on disk" built?)

        ;; The premise, pinned.  It really is a rollback-journal database -- were it
        ;; WAL already, the upgrade asserted below would be satisfied with nothing
        ;; having upgraded anything.
        (test-equal "the legacy file starts out on the rollback journal"
          "delete"
          (journal-mode-of legacy-path))

        (let ([exits (with-watchdog 30
                       (lambda ()
                         (rendezvous "hold-the-read-lock" "open-against-it"
                                     legacy-path)))])

          ;; The appointment was kept, and the session really did open against a lock
          ;; that was really held: the reader took it and committed it (exit 0), and
          ;; the session found the lock still there when it opened (exit 0, not 4).
          ;; Everything below rests on that; fail on the exits first.
          (test-equal "both sessions ran the read-lock rendezvous through to a clean exit"
            '(0 0)
            exits)

          ;; The observation, taken from inside the session rather than on trust.  The
          ;; conversion wanted an exclusive lock and a reader was holding one; given
          ;; the busy timeout FIRST it waits the reader out and the file comes over.
          ;; Asked for before the timeout is set, it is refused instantly, its return
          ;; code is discarded like every other, and the session runs its whole life on
          ;; the rollback journal believing it has WAL.  That is the bug -- and it is a
          ;; fact about the RUNNING session, which is where it is now read.
          ;;
          ;; It cannot be read off the settled file: closing checkpoints the log back
          ;; in and returns the file to the rollback journal, so the correct run and the
          ;; bug would both leave "delete" behind, and asking the file afterwards could
          ;; no longer tell them apart.  What the session leaves behind is asserted in
          ;; its own right elsewhere; this is about what it does.
          (test-equal "a legacy history file opened against a held read lock is still carried over to WAL"
            "wal"
            (recorded-live-mode legacy-path))))

      (remove-db! db-path)
      (remove-db! legacy-path)))

(test-end)
