;;; (hafod internal posix-regex) -- POSIX regex operations
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal posix-regex)
  (export
    posix-regcomp posix-regexec posix-regfree posix-regerror
    REG_EXTENDED REG_ICASE REG_NOSUB REG_NEWLINE
    REG_NOTBOL REG_NOTEOL REG_NOMATCH)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants)
          (hafod internal platform-constants) (hafod internal posix-core))

  ;; ======================================================================
  ;; POSIX Regex (regcomp / regexec / regfree / regerror)
  ;; ======================================================================

  ;; Constants
  (define REG_EXTENDED PLAT-REG-EXTENDED)
  (define REG_ICASE    PLAT-REG-ICASE)
  (define REG_NOSUB    PLAT-REG-NOSUB)
  (define REG_NEWLINE  PLAT-REG-NEWLINE)
  (define REG_NOTBOL   PLAT-REG-NOTBOL)
  (define REG_NOTEOL   PLAT-REG-NOTEOL)
  (define REG_NOMATCH  PLAT-REG-NOMATCH)

  ;; Struct sizes from platform-constants
  (define *regex-t-size* SIZEOF-REGEX-T)
  (define *regmatch-size* SIZEOF-REGMATCH-T)
  (define *regoff-size* SIZEOF-REGOFF-T)

  ;; regoff_t accessor: 4 bytes (Linux) or 8 bytes (FreeBSD/macOS).
  (define regoff-ref
    (if (= *regoff-size* 8)
        (lambda (bv off) (bytevector-s64-native-ref bv off))
        (lambda (bv off) (bytevector-s32-native-ref bv off))))

  ;; FFI declarations
  (define c-regcomp  (foreign-procedure "regcomp"  (u8* string int) int))
  (define c-regexec  (foreign-procedure "regexec"  (u8* string size_t u8* int) int))
  (define c-regfree  (foreign-procedure "regfree"  (u8*) void))
  (define c-regerror (foreign-procedure "regerror" (int u8* u8* size_t) size_t))

  ;; posix-regcomp: compile a POSIX regex string.
  ;; Returns the regex_t bytevector on success, raises error on failure.
  (define (posix-regcomp pattern cflags)
    (let ([rt (make-bytevector *regex-t-size* 0)])
      (let ([ret (c-regcomp rt pattern cflags)])
        (if (= ret 0)
            rt
            (let* ([errbuf (make-bytevector 256 0)]
                   [len (c-regerror ret rt errbuf 256)]
                   [msg (bv-cstring errbuf 0)])
              (c-regfree rt)
              (error 'posix-regcomp msg pattern))))))

  ;; posix-regexec: execute a compiled regex against a string.
  ;; Returns a vector of (start . end) pairs or #f for non-participating submatches,
  ;; or #f if no match.
  (define (posix-regexec rt str nmatch eflags)
    (let* ([pmatch-size (* nmatch *regmatch-size*)]
           [pmatch (make-bytevector pmatch-size 0)]
           [ret (c-regexec rt str nmatch pmatch eflags)])
      (if (= ret 0)
          ;; Match found - extract submatch positions
          (let ([result (make-vector nmatch #f)])
            (let loop ([i 0])
              (when (< i nmatch)
                (let* ([offset (* i *regmatch-size*)]
                       [rm-so (regoff-ref pmatch offset)]
                       [rm-eo (regoff-ref pmatch (+ offset *regoff-size*))])
                  (when (>= rm-so 0)
                    (vector-set! result i (cons rm-so rm-eo)))
                  (loop (+ i 1)))))
            result)
          ;; No match
          #f)))

  ;; posix-regfree: free a compiled regex_t.
  (define (posix-regfree rt)
    (c-regfree rt))

  ;; posix-regerror: get error message for a regex error code.
  (define (posix-regerror errcode rt)
    (let* ([errbuf (make-bytevector 256 0)]
           [len (c-regerror errcode rt errbuf 256)])
      (bv-cstring errbuf 0)))

  ) ; end library
