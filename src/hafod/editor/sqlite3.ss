;;; (hafod editor sqlite3) -- Minimal SQLite3 FFI for history persistence
;;; Provides just enough to open a database, execute SQL, and query rows.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor sqlite3)
  (export sqlite3-open sqlite3-close sqlite3-exec
          sqlite3-prepare sqlite3-finalize sqlite3-reset
          sqlite3-bind-text sqlite3-bind-int64
          sqlite3-step sqlite3-column-text sqlite3-column-int64
          sqlite3-column-count sqlite3-errmsg sqlite3-loaded?
          SQLITE_OK SQLITE_ROW SQLITE_DONE)
  (import (chezscheme) (hafod internal platform))

  ;; Status codes
  (define SQLITE_OK   0)
  (define SQLITE_ROW  100)
  (define SQLITE_DONE 101)

  ;; SQLITE_TRANSIENT tells SQLite to copy bound data immediately
  (define SQLITE_TRANSIENT -1)

  ;; Load libsqlite3 — try platform-appropriate names and common paths.
  ;; Returns #t once a candidate loads, #f otherwise.  Called on first use, not
  ;; at library instantiation, so a non-interactive invocation never dlopens it.
  (define (try-load-sqlite3)
    (let ([try (lambda (name)
                 (guard (e [#t #f])
                   (load-shared-object name) #t))])
      (or (case os-family
            [(macos)
             (or (try "libsqlite3.dylib")
                 (try "/usr/lib/libsqlite3.dylib")
                 (try "/opt/homebrew/lib/libsqlite3.dylib"))]
            [else
             (or (try "libsqlite3.so.0")
                 (try "libsqlite3.so")
                 (try "/usr/lib/x86_64-linux-gnu/libsqlite3.so.0")
                 (try "/usr/lib/aarch64-linux-gnu/libsqlite3.so.0")
                 (try "/usr/lib/libsqlite3.so.0"))])
          #f)))

  ;; Helper: allocate a pointer-sized slot, read/free it.  These use only Chez
  ;; FFI primitives (no shared object), so they stay at library top level.
  (define ptr-size (foreign-sizeof 'void*))
  (define (alloc-ptr-slot) (foreign-alloc ptr-size))
  (define (read-ptr-slot addr)
    (foreign-ref 'void* addr 0))
  (define (free-ptr-slot addr) (foreign-free addr))

  ;; Foreign procedures — resolved on first use, not at instantiation.  Each
  ;; cell is #f until the shared object loads; an unavailable library leaves
  ;; every cell #f, which the wrappers below fall back on exactly as before.
  (define %sqlite3 #f)                 ; #f until ensure-sqlite3! has run once
  (define c-sqlite3-open #f)
  (define c-sqlite3-close #f)
  (define c-sqlite3-exec #f)
  (define c-sqlite3-errmsg #f)
  (define c-sqlite3-prepare-v2 #f)
  (define c-sqlite3-finalize #f)
  (define c-sqlite3-reset #f)
  (define c-sqlite3-step #f)
  (define c-sqlite3-bind-text #f)
  (define c-sqlite3-bind-int64 #f)
  (define c-sqlite3-column-text #f)
  (define c-sqlite3-column-int64 #f)
  (define c-sqlite3-column-count #f)

  ;; First-use initialiser.  The flag is set before the load is attempted so a
  ;; missing library is remembered and not re-probed on every wrapper call.  A
  ;; foreign-procedure resolves its entry point when evaluated, so the bindings
  ;; are built only after the shared object has loaded.  Single-threaded REPL,
  ;; so a bare flag guard is sufficient.
  (define (ensure-sqlite3!)
    (unless %sqlite3
      (set! %sqlite3 #t)
      (when (try-load-sqlite3)
        (set! c-sqlite3-open
          (foreign-procedure "sqlite3_open" (string void*) int))
        (set! c-sqlite3-close
          (foreign-procedure "sqlite3_close" (void*) int))
        (set! c-sqlite3-exec
          (foreign-procedure "sqlite3_exec" (void* string void* void* void*) int))
        (set! c-sqlite3-errmsg
          (foreign-procedure "sqlite3_errmsg" (void*) string))
        (set! c-sqlite3-prepare-v2
          (foreign-procedure "sqlite3_prepare_v2" (void* u8* int void* void*) int))
        (set! c-sqlite3-finalize
          (foreign-procedure "sqlite3_finalize" (void*) int))
        (set! c-sqlite3-reset
          (foreign-procedure "sqlite3_reset" (void*) int))
        (set! c-sqlite3-step
          (foreign-procedure "sqlite3_step" (void*) int))
        (set! c-sqlite3-bind-text
          (foreign-procedure "sqlite3_bind_text" (void* int u8* int void*) int))
        (set! c-sqlite3-bind-int64
          (foreign-procedure "sqlite3_bind_int64" (void* int integer-64) int))
        (set! c-sqlite3-column-text
          (foreign-procedure "sqlite3_column_text" (void* int) string))
        (set! c-sqlite3-column-int64
          (foreign-procedure "sqlite3_column_int64" (void* int) integer-64))
        (set! c-sqlite3-column-count
          (foreign-procedure "sqlite3_column_count" (void*) int)))))

  ;; Probe: #t only once a real C entry point has been resolved (i.e. libsqlite3
  ;; was present and loaded).  Reads the cell without triggering a load, so a
  ;; caller can observe deferral.
  (define (sqlite3-loaded?) (and c-sqlite3-open #t))

  ;; Open a database.  Returns db handle (void* address) or #f on failure.
  (define (sqlite3-open path)
    (ensure-sqlite3!)
    (and c-sqlite3-open
         (let ([slot (alloc-ptr-slot)])
           (let ([rc (c-sqlite3-open path slot)])
             (let ([db (read-ptr-slot slot)])
               (free-ptr-slot slot)
               (if (= rc SQLITE_OK) db #f))))))

  ;; Close a database handle.
  (define (sqlite3-close db)
    (ensure-sqlite3!)
    (when (and db c-sqlite3-close)
      (c-sqlite3-close db)))

  ;; Execute SQL with no result rows (DDL, INSERT, etc.).
  ;; Returns SQLITE_OK on success.
  (define (sqlite3-exec db sql)
    (ensure-sqlite3!)
    (if c-sqlite3-exec
        (c-sqlite3-exec db sql 0 0 0)
        1))

  ;; Get error message for last operation on db.
  (define (sqlite3-errmsg db)
    (ensure-sqlite3!)
    (if (and db c-sqlite3-errmsg)
        (c-sqlite3-errmsg db)
        "sqlite3 not available"))

  ;; Prepare a statement.  Returns stmt handle (void* address) or #f.
  (define (sqlite3-prepare db sql)
    (ensure-sqlite3!)
    (and c-sqlite3-prepare-v2
         (let* ([slot (alloc-ptr-slot)]
                [bv (string->utf8 sql)]
                [rc (c-sqlite3-prepare-v2 db bv (bytevector-length bv) slot 0)])
           (let ([stmt (read-ptr-slot slot)])
             (free-ptr-slot slot)
             (if (= rc SQLITE_OK) stmt #f)))))

  ;; Finalize a statement.
  (define (sqlite3-finalize stmt)
    (ensure-sqlite3!)
    (when (and stmt c-sqlite3-finalize)
      (c-sqlite3-finalize stmt)))

  ;; Reset a statement for re-execution.
  (define (sqlite3-reset stmt)
    (ensure-sqlite3!)
    (when (and stmt c-sqlite3-reset)
      (c-sqlite3-reset stmt)))

  ;; Bind text to parameter index (1-based).
  (define (sqlite3-bind-text stmt idx text)
    (ensure-sqlite3!)
    (if c-sqlite3-bind-text
        (let ([bv (string->utf8 text)])
          (c-sqlite3-bind-text stmt idx bv (bytevector-length bv) SQLITE_TRANSIENT))
        1))

  ;; Bind int64 to parameter index (1-based).
  (define (sqlite3-bind-int64 stmt idx val)
    (ensure-sqlite3!)
    (if c-sqlite3-bind-int64
        (c-sqlite3-bind-int64 stmt idx val)
        1))

  ;; Step a statement.  Returns SQLITE_ROW, SQLITE_DONE, or error code.
  (define (sqlite3-step stmt)
    (ensure-sqlite3!)
    (if c-sqlite3-step
        (c-sqlite3-step stmt)
        SQLITE_DONE))

  ;; Get text from column (0-based) of current row.
  (define (sqlite3-column-text stmt col)
    (ensure-sqlite3!)
    (if c-sqlite3-column-text
        (c-sqlite3-column-text stmt col)
        ""))

  ;; Get int64 from column (0-based) of current row.
  (define (sqlite3-column-int64 stmt col)
    (ensure-sqlite3!)
    (if c-sqlite3-column-int64
        (c-sqlite3-column-int64 stmt col)
        0))

  ;; Get column count from a statement.
  (define (sqlite3-column-count stmt)
    (ensure-sqlite3!)
    (if c-sqlite3-column-count
        (c-sqlite3-column-count stmt)
        0))

) ; end library
