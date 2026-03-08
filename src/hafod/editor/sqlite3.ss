;;; (hafod editor sqlite3) -- Minimal SQLite3 FFI for history persistence
;;; Provides just enough to open a database, execute SQL, and query rows.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod editor sqlite3)
  (export sqlite3-open sqlite3-close sqlite3-exec
          sqlite3-prepare sqlite3-finalize sqlite3-reset
          sqlite3-bind-text sqlite3-bind-int64
          sqlite3-step sqlite3-column-text sqlite3-column-int64
          sqlite3-column-count sqlite3-errmsg
          SQLITE_OK SQLITE_ROW SQLITE_DONE)
  (import (chezscheme))

  ;; Status codes
  (define SQLITE_OK   0)
  (define SQLITE_ROW  100)
  (define SQLITE_DONE 101)

  ;; SQLITE_TRANSIENT tells SQLite to copy bound data immediately
  (define SQLITE_TRANSIENT -1)

  ;; Load libsqlite3 — try platform-appropriate names and common paths
  (define load-sqlite3
    (let ([try (lambda (name)
                 (guard (e [#t #f])
                   (load-shared-object name) #t))])
      (or (case (machine-type)
            [(ta6osx tarm64osx ti3osx a6osx arm64osx i3osx)
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

  ;; Helper: allocate a pointer-sized slot, read/free it
  (define ptr-size (foreign-sizeof 'void*))
  (define (alloc-ptr-slot) (foreign-alloc ptr-size))
  (define (read-ptr-slot addr)
    (foreign-ref 'void* addr 0))
  (define (free-ptr-slot addr) (foreign-free addr))

  ;; Foreign procedures
  (define c-sqlite3-open
    (and load-sqlite3
         (foreign-procedure "sqlite3_open" (string void*) int)))
  (define c-sqlite3-close
    (and load-sqlite3
         (foreign-procedure "sqlite3_close" (void*) int)))
  (define c-sqlite3-exec
    (and load-sqlite3
         (foreign-procedure "sqlite3_exec" (void* string void* void* void*) int)))
  (define c-sqlite3-errmsg
    (and load-sqlite3
         (foreign-procedure "sqlite3_errmsg" (void*) string)))
  (define c-sqlite3-prepare-v2
    (and load-sqlite3
         (foreign-procedure "sqlite3_prepare_v2" (void* u8* int void* void*) int)))
  (define c-sqlite3-finalize
    (and load-sqlite3
         (foreign-procedure "sqlite3_finalize" (void*) int)))
  (define c-sqlite3-reset
    (and load-sqlite3
         (foreign-procedure "sqlite3_reset" (void*) int)))
  (define c-sqlite3-step
    (and load-sqlite3
         (foreign-procedure "sqlite3_step" (void*) int)))
  (define c-sqlite3-bind-text
    (and load-sqlite3
         (foreign-procedure "sqlite3_bind_text" (void* int u8* int void*) int)))
  (define c-sqlite3-bind-int64
    (and load-sqlite3
         (foreign-procedure "sqlite3_bind_int64" (void* int integer-64) int)))
  (define c-sqlite3-column-text
    (and load-sqlite3
         (foreign-procedure "sqlite3_column_text" (void* int) string)))
  (define c-sqlite3-column-int64
    (and load-sqlite3
         (foreign-procedure "sqlite3_column_int64" (void* int) integer-64)))
  (define c-sqlite3-column-count
    (and load-sqlite3
         (foreign-procedure "sqlite3_column_count" (void*) int)))

  ;; Open a database.  Returns db handle (void* address) or #f on failure.
  (define (sqlite3-open path)
    (and c-sqlite3-open
         (let ([slot (alloc-ptr-slot)])
           (let ([rc (c-sqlite3-open path slot)])
             (let ([db (read-ptr-slot slot)])
               (free-ptr-slot slot)
               (if (= rc SQLITE_OK) db #f))))))

  ;; Close a database handle.
  (define (sqlite3-close db)
    (when (and db c-sqlite3-close)
      (c-sqlite3-close db)))

  ;; Execute SQL with no result rows (DDL, INSERT, etc.).
  ;; Returns SQLITE_OK on success.
  (define (sqlite3-exec db sql)
    (if c-sqlite3-exec
        (c-sqlite3-exec db sql 0 0 0)
        1))

  ;; Get error message for last operation on db.
  (define (sqlite3-errmsg db)
    (if (and db c-sqlite3-errmsg)
        (c-sqlite3-errmsg db)
        "sqlite3 not available"))

  ;; Prepare a statement.  Returns stmt handle (void* address) or #f.
  (define (sqlite3-prepare db sql)
    (and c-sqlite3-prepare-v2
         (let* ([slot (alloc-ptr-slot)]
                [bv (string->utf8 sql)]
                [rc (c-sqlite3-prepare-v2 db bv (bytevector-length bv) slot 0)])
           (let ([stmt (read-ptr-slot slot)])
             (free-ptr-slot slot)
             (if (= rc SQLITE_OK) stmt #f)))))

  ;; Finalize a statement.
  (define (sqlite3-finalize stmt)
    (when (and stmt c-sqlite3-finalize)
      (c-sqlite3-finalize stmt)))

  ;; Reset a statement for re-execution.
  (define (sqlite3-reset stmt)
    (when (and stmt c-sqlite3-reset)
      (c-sqlite3-reset stmt)))

  ;; Bind text to parameter index (1-based).
  (define (sqlite3-bind-text stmt idx text)
    (if c-sqlite3-bind-text
        (let ([bv (string->utf8 text)])
          (c-sqlite3-bind-text stmt idx bv (bytevector-length bv) SQLITE_TRANSIENT))
        1))

  ;; Bind int64 to parameter index (1-based).
  (define (sqlite3-bind-int64 stmt idx val)
    (if c-sqlite3-bind-int64
        (c-sqlite3-bind-int64 stmt idx val)
        1))

  ;; Step a statement.  Returns SQLITE_ROW, SQLITE_DONE, or error code.
  (define (sqlite3-step stmt)
    (if c-sqlite3-step
        (c-sqlite3-step stmt)
        SQLITE_DONE))

  ;; Get text from column (0-based) of current row.
  (define (sqlite3-column-text stmt col)
    (if c-sqlite3-column-text
        (c-sqlite3-column-text stmt col)
        ""))

  ;; Get int64 from column (0-based) of current row.
  (define (sqlite3-column-int64 stmt col)
    (if c-sqlite3-column-int64
        (c-sqlite3-column-int64 stmt col)
        0))

  ;; Get column count from a statement.
  (define (sqlite3-column-count stmt)
    (if c-sqlite3-column-count
        (c-sqlite3-column-count stmt)
        0))

) ; end library
