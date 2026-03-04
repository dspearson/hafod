;;; (hafod lib-dirs) -- Library search path management
;;; Manages a list of directories for finding library files.
;;; Ported from scsh/scheme/lib-dirs.scm (119 LOC).
;;; Copyright (c) 1994-2003 Olin Shivers. R6RS adaptation (c) 2026 Dominic Pearson.

(library (hafod lib-dirs)
  (export lib-dirs lib-dirs-prepend! lib-dirs-append!
          reset-lib-dirs! clear-lib-dirs! find-library-file
          default-lib-dirs
          lib-dirs-append-script-dir! lib-dirs-prepend-script-dir!)

  (import (chezscheme)
          (only (hafod posix) posix-getenv))

  ;; ======================================================================
  ;; Default library directories
  ;; ======================================================================

  (define *default-lib-dirs* '("/usr/local/lib/hafod/modules/"))

  ;; Vector-box for mutable state (lazy initialization).
  ;; #f = not yet initialized (will parse env var on first access).
  (define *lib-dirs-box* (vector #f))

  ;; ======================================================================
  ;; Environment variable parsing
  ;; ======================================================================

  ;; Parse $HAFOD_LIB_DIRS env var.
  ;; Format: space-separated Scheme datums read via `read`.
  ;; Strings are directory paths. #f means "insert defaults here".
  ;; If env var is not set, return *default-lib-dirs*.
  (define (parse-lib-dirs-env-var)
    (let ([s (posix-getenv "HAFOD_LIB_DIRS")])
      (if (not s)
          *default-lib-dirs*
          (let ([p (open-input-string s)])
            (let loop ()
              (let ([val (read p)])
                (cond
                  [(eof-object? val) '()]
                  [(string? val) (cons val (loop))]
                  [(not val) (append *default-lib-dirs* (loop))]
                  [else (error 'parse-lib-dirs-env-var
                          "illegal element in $HAFOD_LIB_DIRS" val)])))))))

  ;; ======================================================================
  ;; Core API
  ;; ======================================================================

  ;; lib-dirs: return current search path (lazy init on first call).
  (define (lib-dirs)
    (let ([v (vector-ref *lib-dirs-box* 0)])
      (or v
          (let ([dirs (parse-lib-dirs-env-var)])
            (vector-set! *lib-dirs-box* 0 dirs)
            dirs))))

  ;; Internal setter.
  (define (set-lib-dirs! val)
    (vector-set! *lib-dirs-box* 0 val))

  ;; Prepend a directory to the front of the search path.
  (define (lib-dirs-prepend! dir)
    (set-lib-dirs! (cons dir (lib-dirs))))

  ;; Append a directory to the end of the search path.
  (define (lib-dirs-append! dir)
    (set-lib-dirs! (append (lib-dirs) (list dir))))

  ;; Reset to default search path.
  (define (reset-lib-dirs!)
    (set-lib-dirs! *default-lib-dirs*))

  ;; Clear search path to empty list.
  (define (clear-lib-dirs!)
    (set-lib-dirs! '()))

  ;; Return default library directories list.
  (define (default-lib-dirs) *default-lib-dirs*)

  ;; Script directory tracking (set by the launcher).
  (define *script-dir-box* (vector #f))

  ;; Append the script's directory to lib-dirs.
  (define (lib-dirs-append-script-dir!)
    (let ([dir (vector-ref *script-dir-box* 0)])
      (when dir (lib-dirs-append! dir))))

  ;; Prepend the script's directory to lib-dirs.
  (define (lib-dirs-prepend-script-dir!)
    (let ([dir (vector-ref *script-dir-box* 0)])
      (when dir (lib-dirs-prepend! dir))))

  ;; ======================================================================
  ;; find-library-file
  ;; ======================================================================

  ;; Search lib-dirs for a file. Returns full path or #f.
  ;; Simplified from scsh (no script-file support, no recursive subdirectory search).
  (define (find-library-file file)
    (let loop ([dirs (lib-dirs)])
      (if (null? dirs)
          #f
          (let* ([dir (car dirs)]
                 [dir-s (if (and (> (string-length dir) 0)
                                 (char=? (string-ref dir (- (string-length dir) 1)) #\/))
                            dir
                            (string-append dir "/"))]
                 [path (string-append dir-s file)])
            (if (guard (e [#t #f]) (file-exists? path))
                path
                (loop (cdr dirs)))))))

) ;; end library
