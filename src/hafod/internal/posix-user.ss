;;; (hafod internal posix-user) -- User/group database operations
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal posix-user)
  (export
    posix-getpwnam posix-getpwuid posix-getgrnam posix-getgrgid
    passwd-info? passwd-info-name passwd-info-passwd passwd-info-uid
    passwd-info-gid passwd-info-gecos passwd-info-dir passwd-info-shell
    group-info? group-info-name group-info-passwd group-info-gid group-info-members)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants)
          (hafod internal platform-constants) (hafod internal posix-core))

  ;; ======================================================================
  ;; Struct extraction macros
  ;; ======================================================================

  ;; extract-field: dispatch on type keyword to appropriate foreign-ref call.
  (define-syntax extract-field
    (syntax-rules (string u32 int long)
      [(_ string ptr offset)  (ptr->string (foreign-ref 'uptr ptr offset))]
      [(_ u32 ptr offset)     (foreign-ref 'unsigned-32 ptr offset)]
      [(_ int ptr offset)     (foreign-ref 'int ptr offset)]
      [(_ long ptr offset)    (foreign-ref 'long ptr offset)]))

  ;; define-struct-extractor: generate a field extraction function from
  ;; declarative (type offset) specifications.
  (define-syntax define-struct-extractor
    (syntax-rules ()
      [(_ extractor-name constructor ptr-var (type offset) ...)
       (define (extractor-name ptr-var)
         (constructor
           (extract-field type ptr-var offset) ...))]))

  ;; ======================================================================
  ;; User/group database
  ;; ======================================================================

  ;; struct group offsets from platform-constants
  (define GR_NAME    GR-NAME)
  (define GR_PASSWD  GR-PASSWD)
  (define GR_GID     GR-GID)
  (define GR_MEM     GR-MEM)

  (define c-getpwnam (foreign-procedure "getpwnam" (string) void*))
  (define c-getpwuid (foreign-procedure "getpwuid" (unsigned-32) void*))
  (define c-getgrnam (foreign-procedure "getgrnam" (string) void*))
  (define c-getgrgid (foreign-procedure "getgrgid" (unsigned-32) void*))

  ;; passwd-info record type
  (define-record-type passwd-info
    (fields name passwd uid gid gecos dir shell))

  ;; group-info record type
  (define-record-type group-info
    (fields name passwd gid members))

  ;; Extract passwd-info from a struct passwd pointer (macro-generated).
  (define-struct-extractor extract-passwd-info make-passwd-info ptr
    (string PW-NAME)
    (string PW-PASSWD)
    (u32    PW-UID)
    (u32    PW-GID)
    (string PW-GECOS)
    (string PW-DIR)
    (string PW-SHELL))

  ;; Extract group member list from gr_mem (char** array).
  (define (extract-group-members grp-ptr)
    (let ([mem-array (foreign-ref 'uptr grp-ptr GR_MEM)])
      (if (= mem-array 0) '()
          (let loop ([i 0] [members '()])
            (let ([str-ptr (foreign-ref 'uptr mem-array (* i (foreign-sizeof 'void*)))])
              (if (= str-ptr 0)
                  (reverse members)
                  (loop (+ i 1) (cons (ptr->string str-ptr) members))))))))

  ;; Extract group-info from a struct group pointer.
  (define (extract-group-info ptr)
    (make-group-info
      (ptr->string (foreign-ref 'uptr ptr GR_NAME))
      (ptr->string (foreign-ref 'uptr ptr GR_PASSWD))
      (foreign-ref 'unsigned-32 ptr GR_GID)
      (extract-group-members ptr)))

  ;; getpwnam: look up user by name. Returns passwd-info or #f.
  (define (posix-getpwnam name)
    (let ([ptr (c-getpwnam name)])
      (if (= ptr 0) #f (extract-passwd-info ptr))))

  ;; getpwuid: look up user by uid. Returns passwd-info or #f.
  (define (posix-getpwuid uid)
    (let ([ptr (c-getpwuid uid)])
      (if (= ptr 0) #f (extract-passwd-info ptr))))

  ;; getgrnam: look up group by name. Returns group-info or #f.
  (define (posix-getgrnam name)
    (let ([ptr (c-getgrnam name)])
      (if (= ptr 0) #f (extract-group-info ptr))))

  ;; getgrgid: look up group by gid. Returns group-info or #f.
  (define (posix-getgrgid gid)
    (let ([ptr (c-getgrgid gid)])
      (if (= ptr 0) #f (extract-group-info ptr))))

  ) ; end library
