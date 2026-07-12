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
  ;; REG_STARTEND is deliberately kept internal (not in the library exports):
  ;; it is an implementation detail of the offset-seeded exec path below.
  (define REG_STARTEND PLAT-REG-STARTEND)

  ;; Struct sizes from platform-constants
  (define *regex-t-size* SIZEOF-REGEX-T)
  (define *regmatch-size* SIZEOF-REGMATCH-T)
  (define *regoff-size* SIZEOF-REGOFF-T)

  ;; regoff_t accessor: 4 bytes (Linux) or 8 bytes (FreeBSD/macOS).
  (define regoff-ref
    (if (= *regoff-size* 8)
        (lambda (bv off) (bytevector-s64-native-ref bv off))
        (lambda (bv off) (bytevector-s32-native-ref bv off))))

  ;; regoff_t writer: mirrors regoff-ref, used to seed pmatch[0] for the
  ;; start/end matching path below (the same s32/s64 platform pick).
  (define regoff-set!
    (if (= *regoff-size* 8)
        (lambda (bv off val) (bytevector-s64-native-set! bv off val))
        (lambda (bv off val) (bytevector-s32-native-set! bv off val))))

  ;; regcomp/regexec resolve bracket ranges (e.g. [\x01-\x7f]) through the libc
  ;; LC_COLLATE category. hafod pins LC_COLLATE to "C" at process init (see the
  ;; locale note in (hafod editor input-decode)) so those ranges compare by
  ;; byte/codepoint value regardless of the user's collation, rather than as a
  ;; locale collation range that could exclude plain ASCII letters. Case-folding
  ;; (REG_ICASE) and named classes ([[:alpha:]]) still follow LC_CTYPE, which
  ;; stays the environment's UTF-8 locale.
  ;; FFI declarations
  (define c-regcomp  (foreign-procedure "regcomp"  (u8* string int) int))
  (define c-regexec  (foreign-procedure "regexec"  (u8* u8* size_t u8* int) int))
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

  ;; posix-regexec: execute a compiled regex against a subject.
  ;; SUBJECT may be a Scheme string (encoded to UTF-8 here) or a pre-encoded
  ;; bytevector, so an iterating caller can encode the subject once and search it
  ;; from many offsets without re-encoding. The optional START/END are byte
  ;; offsets into the buffer; they seed pmatch[0] so matching runs over the slice
  ;; [start,end) via REG_STARTEND. Returned offsets are ABSOLUTE (relative to the
  ;; buffer base, not the slice), and REG_NOTBOL is set when start>0 so `^`/bos
  ;; does not falsely match at a mid-buffer offset. Returns a vector of
  ;; (start . end) pairs (#f per non-participating submatch), or #f if no match.
  (define (posix-regexec rt subject nmatch eflags . maybe-start-end)
    (let* ([buf (if (bytevector? subject) subject (string->utf8 subject))]
           [blen (bytevector-length buf)]
           [start (if (pair? maybe-start-end) (car maybe-start-end) 0)]
           [end (if (and (pair? maybe-start-end) (pair? (cdr maybe-start-end)))
                    (cadr maybe-start-end)
                    blen)]
           ;; REG_STARTEND always needs pmatch[0] to seed the search slice, even
           ;; when the caller wants no submatch groups (nmatch = 0) as a bare
           ;; "does it match at all?" query. Allocate at least one slot for the
           ;; seed; the returned vector below still reports exactly nmatch groups.
           [slots (if (< nmatch 1) 1 nmatch)]
           [pmatch (make-bytevector (* slots *regmatch-size*) 0)]
           [eff-eflags (bitwise-ior REG_STARTEND eflags (if (> start 0) REG_NOTBOL 0))])
      ;; Bound-check the seeded slice against the subject buffer before handing
      ;; it to regexec. REG_STARTEND treats rm_eo as the total length the engine
      ;; may read, so an end past the buffer -- or an inverted/negative slice --
      ;; would be an out-of-bounds read over the bytevector. Reject anything that
      ;; is not a valid [start,end) within [0, blen] rather than trusting the
      ;; caller; the common whole-buffer case (end = blen, start in range) is
      ;; unaffected.
      (when (or (< start 0) (> end blen) (> start end))
        (error 'posix-regexec "match offsets out of range" start end blen))
      ;; Seed pmatch[0] with the search slice; REG_STARTEND reads these bounds.
      (regoff-set! pmatch 0 start)
      (regoff-set! pmatch *regoff-size* end)
      (let ([ret (c-regexec rt buf slots pmatch eff-eflags)])
        (if (= ret 0)
            ;; Match found - extract submatch positions (offsets are absolute)
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
            #f))))

  ;; posix-regfree: free a compiled regex_t.
  (define (posix-regfree rt)
    (c-regfree rt))

  ;; posix-regerror: get error message for a regex error code.
  (define (posix-regerror errcode rt)
    (let* ([errbuf (make-bytevector 256 0)]
           [len (c-regerror errcode rt errbuf 256)])
      (bv-cstring errbuf 0)))

  ) ; end library
