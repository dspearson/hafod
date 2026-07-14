;;; (hafod internal re-engine) -- Compiled regex engine for hafod
;;; Extracted from (hafod re) -- compiled-regexp record, match record,
;;; search/match operations, substitution, and fold/iteration.
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod internal re-engine)
  (export
    ;; Public exports (re-exported by re.ss facade)
    regexp? make-regexp string->regexp
    regexp-search regexp-search? regexp-match
    match:start match:end match:substring match:count
    regexp-match?
    regexp-substitute regexp-substitute/global
    regexp-fold regexp-for-each regexp-fold-right
    re-adt->compiled-regexp
    ;; Internal exports (needed by re-macros, NOT re-exported by facade)
    coerce-regexp finalize-regexes!
    compiled-regexp-type? compiled-regexp-type-num-submatches
    compiled-regexp-type-regex-t compiled-regexp-type-submatch-map
    compiled-regexp-type-posix-string
    count-posix-parens
    regexp-match-type-string regexp-match-type-submatches
    build-match-vector
    ;; Line-aware compilation seam (shared with re-macros; NOT re-exported by
    ;; the facade, so the umbrella surface does not grow). The probe is exported
    ;; alongside the pure predicate so a caller -- in practice the anchor test
    ;; suites -- can ask what THIS libc can express, rather than guessing from
    ;; the platform name.
    line-aware-anchors-ok? gnu-buffer-anchors-available? make-line-aware-regexp)
  (import (hafod internal base)
          (hafod posix)
          (hafod compat)
          (rename (only (hafod internal re-records) regexp?)
                  (regexp? re-adt?))
          (only (hafod internal re-posixstr)
                regexp->posix-string re-line-aware? re-has-string-anchor?))

  ;; ======================================================================
  ;; Compiled regex record
  ;; ======================================================================

  ;; submatch-map: a vector where element i (0-based) is the POSIX 1-based
  ;; paren index for user submatch i. #f means identity mapping (no remapping).
  ;; For string->regexp (no SRE), submatch-map is #f (identity).
  (define-record-type compiled-regexp-type
    (fields
      posix-string
      cflags
      (mutable regex-t)
      num-submatches
      submatch-map)
    (nongenerative hafod-compiled-regexp))

  ;; Guardian for regex_t cleanup
  (define regex-guardian (make-guardian))

  (define (finalize-regexes!)
    (let loop ()
      (let ((crx (regex-guardian)))
        (when crx
          (let ((rt (compiled-regexp-type-regex-t crx)))
            (when rt (posix-regfree rt)))
          (loop)))))

  ;; Create a compiled regexp, eagerly compiling the POSIX string.
  ;; smap is a vector mapping user submatch indices to POSIX paren indices,
  ;; or #f for identity mapping.
  (define make-regexp
    (case-lambda
      ((posix-str cflags nsub)
       (make-regexp posix-str cflags nsub #f))
      ((posix-str cflags nsub smap)
       (finalize-regexes!)
       (let* ((rt (posix-regcomp posix-str (bitwise-ior REG_EXTENDED cflags)))
              (crx (make-compiled-regexp-type posix-str cflags rt nsub smap)))
         (regex-guardian crx)
         crx))))

  ;; ======================================================================
  ;; Line-aware compilation: REG_NEWLINE + the fail-loud capability seam
  ;; ======================================================================
  ;; bol/eol match at LINE boundaries only when the compiled regex carries
  ;; REG_NEWLINE, which the ADT/rx compilers request via a line-aware signal.
  ;; Keeping bos/eos STRING-anchored under REG_NEWLINE needs glibc's GNU buffer
  ;; anchors, which do not exist on a BSD/musl libc. A pattern that mixes bos/eos
  ;; with bol/eol therefore cannot be expressed correctly everywhere, so it must
  ;; FAIL LOUD where those anchors are absent rather than silently match a
  ;; literal backtick/apostrophe.

  ;; Memoised probe: does the running libc treat the buffer-start anchor as an
  ;; anchor? Compile it and match against "x": a GNU libc reads it as a
  ;; zero-width buffer-start anchor (a match), while a BSD/musl libc reads it as
  ;; a literal backtick, absent from "x" (no match). The libc cannot change
  ;; within a process, so the answer is cached after the first call. Any
  ;; regcomp/regexec failure is treated as "unavailable". Single-threaded, so a
  ;; plain variable needs no lock (mirrors the *regexp-cache* idiom above).
  (define *gnu-buffer-anchors* 'unprobed)
  (define (gnu-buffer-anchors-available?)
    (when (eq? *gnu-buffer-anchors* 'unprobed)
      (set! *gnu-buffer-anchors*
        (guard (exn [#t #f])
          (let ((rt (posix-regcomp (string #\\ #\`) REG_EXTENDED)))
            (let ((m (posix-regexec rt "x" 1 0)))
              (posix-regfree rt)
              (and m #t))))))
    *gnu-buffer-anchors*)

  ;; Pure capability predicate (the testable seam). A line-aware pattern is
  ;; expressible iff it uses no string anchor, or the libc has the GNU buffer
  ;; anchors. A pattern that is not line-aware is always fine.
  (define (line-aware-anchors-ok? line-aware? has-string-anchor? gnu-available?)
    (or (not line-aware?) (not has-string-anchor?) gnu-available?))

  ;; Build a line-aware compiled regex (REG_NEWLINE folded into cflags), failing
  ;; loud when a mixed string+line anchor pattern needs GNU buffer anchors the
  ;; running libc lacks.
  (define (make-line-aware-regexp posix-str cflags nsub smap has-string-anchor?)
    (unless (line-aware-anchors-ok? #t has-string-anchor?
                                    (gnu-buffer-anchors-available?))
      (error 'make-line-aware-regexp
             "line-anchored pattern also uses bos/eos, which needs GNU buffer anchors this libc lacks"))
    (make-regexp posix-str cflags nsub smap))

  ;; ======================================================================
  ;; Bounded compiled-regexp cache (guardian-safe)
  ;; ======================================================================
  ;; A raw string pattern searched repeatedly in a loop should be compiled
  ;; ONCE, not re-compiled on every call. This table, consulted at the
  ;; string->regexp boundary, memoises (pattern . flags) -> compiled record.
  ;;
  ;; Lifetime rule (do NOT break it): the table holds a STRONG reference, so a
  ;; cached record is always reachable and the guardian above never reclaims it
  ;; while it is cached. Eviction drops that strong reference with
  ;; hashtable-delete! ONLY -- it must NEVER free the underlying regex. The
  ;; guardian (finalize-regexes!) frees each compiled regex exactly once, when
  ;; its record has become fully unreachable; a second free on eviction would be
  ;; a double-free, and freeing a record a caller still holds would be a
  ;; use-after-free. Insertion-order eviction bounds the table under a loop of
  ;; distinct patterns. Every symbol here stays internal (unexported) so the
  ;; umbrella surface does not grow. Single-threaded, so a plain hashtable needs
  ;; no lock. (Shape modelled on procobj's *process-table*.)
  (define regexp-cache-capacity 256)
  (define *regexp-cache* (make-hashtable equal-hash equal?))
  ;; Keys in insertion order, oldest first; the head is the eviction victim.
  (define *regexp-cache-order* '())

  (define (regexp-cache-ref key)
    (hashtable-ref *regexp-cache* key #f))

  (define (regexp-cache-evict-oldest!)
    (when (pair? *regexp-cache-order*)
      (let ((victim (car *regexp-cache-order*)))
        (set! *regexp-cache-order* (cdr *regexp-cache-order*))
        ;; Drop the strong ref ONLY. The guardian frees the compiled regex once
        ;; the record is unreachable; never free it here (double-free / UAF).
        (hashtable-delete! *regexp-cache* victim))))

  (define (regexp-cache-insert! key crx)
    (when (>= (hashtable-size *regexp-cache*) regexp-cache-capacity)
      (regexp-cache-evict-oldest!))
    (hashtable-set! *regexp-cache* key crx)
    (set! *regexp-cache-order* (append *regexp-cache-order* (list key)))
    crx)

  ;; Convenience: compile from a POSIX regex string.
  ;; Counts unescaped parens to determine number of submatches. Compiled records
  ;; are memoised on (pattern . flags) so a pattern reused in a hot loop is
  ;; compiled once (see the cache note above).
  (define (string->regexp pattern . maybe-flags)
    (let* ((extra-flags (if (null? maybe-flags) 0 (car maybe-flags)))
           (key (cons pattern extra-flags)))
      (or (regexp-cache-ref key)
          (regexp-cache-insert!
            key
            (make-regexp pattern extra-flags (count-posix-parens pattern) #f)))))

  ;; Count unescaped ( in a POSIX regex string (runtime version).
  (define (count-posix-parens s)
    (let ((len (string-length s)))
      (let loop ((i 0) (n 0))
        (if (>= i len)
            n
            (let ((c (string-ref s i)))
              (cond
                ((and (char=? c #\\) (< (+ i 1) len))
                 (loop (+ i 2) n))
                ((char=? c #\()
                 (loop (+ i 1) (+ n 1)))
                (else
                 (loop (+ i 1) n))))))))

  ;; Predicate: recognizes both compiled regexps and RE ADT values
  (define (regexp? x) (or (compiled-regexp-type? x) (re-adt? x)))

  ;; ======================================================================
  ;; Match object record
  ;; ======================================================================

  (define-record-type regexp-match-type
    (fields
      string
      submatches)
    (nongenerative hafod-regexp-match))

  (define (regexp-match? x) (regexp-match-type? x))

  (define (match:start m . maybe-i)
    (let* ((i (if (null? maybe-i) 0 (car maybe-i)))
           (subs (regexp-match-type-submatches m)))
      (and (< i (vector-length subs))
           (let ((sm (vector-ref subs i)))
             (and sm (car sm))))))

  (define (match:end m . maybe-i)
    (let* ((i (if (null? maybe-i) 0 (car maybe-i)))
           (subs (regexp-match-type-submatches m)))
      (and (< i (vector-length subs))
           (let ((sm (vector-ref subs i)))
             (and sm (cdr sm))))))

  (define (match:substring m . maybe-i)
    (let* ((i (if (null? maybe-i) 0 (car maybe-i)))
           (subs (regexp-match-type-submatches m)))
      (and (< i (vector-length subs))
           (let ((sm (vector-ref subs i)))
             (and sm (substring (regexp-match-type-string m) (car sm) (cdr sm)))))))

  (define (match:count m)
    (vector-length (regexp-match-type-submatches m)))

  ;; ======================================================================
  ;; Search operations
  ;; ======================================================================

  ;; Build a user-facing match vector from raw POSIX regexec results.
  ;; smap is a vector mapping user submatch indices (0-based) to POSIX paren
  ;; indices (1-based), or #f for identity mapping.
  ;; User submatch 0 is always the whole match (POSIX index 0).
  (define (build-match-vector raw-result smap start)
    (if (not smap)
        ;; Identity mapping: user index i = POSIX index i
        (let ((v (make-vector (vector-length raw-result) #f)))
          (let loop ((i 0))
            (when (< i (vector-length raw-result))
              (let ((pair (vector-ref raw-result i)))
                (when pair
                  (vector-set! v i (cons (+ start (car pair))
                                         (+ start (cdr pair))))))
              (loop (+ i 1))))
          v)
        ;; Remapped: user submatch 0 = POSIX 0 (whole match),
        ;; user submatch k = POSIX smap[k-1] for k >= 1
        (let* ((n-user (+ 1 (vector-length smap)))
               (v (make-vector n-user #f)))
          ;; User index 0 = whole match = POSIX index 0
          (let ((pair (vector-ref raw-result 0)))
            (when pair
              (vector-set! v 0 (cons (+ start (car pair))
                                     (+ start (cdr pair))))))
          ;; User index k (1-based) maps to POSIX index smap[k-1]
          ;; posix-idx can be #f for dead submatches (DSM)
          (let loop ((k 1))
            (when (<= k (vector-length smap))
              (let ((posix-idx (vector-ref smap (- k 1))))
                (when (and posix-idx (< posix-idx (vector-length raw-result)))
                  (let ((pair (vector-ref raw-result posix-idx)))
                    (when pair
                      (vector-set! v k (cons (+ start (car pair))
                                             (+ start (cdr pair))))))))
              (loop (+ k 1))))
          v)))

  ;; Bridge: compile an RE ADT value to a compiled-regexp.
  (define (re-adt->compiled-regexp re)
    (let ((line-aware? (re-line-aware? re))
          (has-anchor? (re-has-string-anchor? re)))
      (let-values (((posix-str level pcount smap) (regexp->posix-string re)))
        (if (not posix-str)
            (error 're-adt->compiled-regexp "RE can never match" re)
            (let ((smap (if (= 0 (vector-length smap)) #f smap)))
              (if line-aware?
                  ;; Line-aware: set REG_NEWLINE and route the mixed string+line
                  ;; anchor case through the fail-loud seam.
                  (make-line-aware-regexp posix-str REG_NEWLINE pcount smap
                                          has-anchor?)
                  ;; Not line-aware: byte-identical to the historic path
                  ;; (cflags 0, no REG_NEWLINE).
                  (make-regexp posix-str 0 pcount smap)))))))

  ;; Coerce any regex representation to compiled-regexp-type.
  (define (coerce-regexp re)
    (cond
     ((compiled-regexp-type? re) re)
     ((re-adt? re) (re-adt->compiled-regexp re))
     ((string? re) (string->regexp re))
     (else (error 'coerce-regexp "not a regexp, RE ADT, or string" re))))

  ;; Internal search primitive over a pre-encoded UTF-8 buffer. STR is the
  ;; original string (kept by the match object for match:substring), BUF is
  ;; (string->utf8 str), and START is a byte offset. A caller that iterates
  ;; encodes BUF once and only varies START, so the encode is not repeated per
  ;; step. The subject is searched over [start, length); posix-regexec returns
  ;; absolute (buffer-relative) offsets, so the match vector is rebased with 0.
  (define (regexp-search/bytes crx str buf start)
    (let* ((nsub (+ 1 (compiled-regexp-type-num-submatches crx)))
           (result (posix-regexec (compiled-regexp-type-regex-t crx)
                                  buf nsub 0 start (bytevector-length buf))))
      (and result
           (make-regexp-match-type
             str
             (build-match-vector result
                                 (compiled-regexp-type-submatch-map crx)
                                 0)))))

  (define (regexp-search re str . maybe-start)
    (finalize-regexes!)
    (let* ((crx (coerce-regexp re))
           (start (if (null? maybe-start) 0 (car maybe-start)))
           (buf (string->utf8 str)))
      ;; Restore the fail-loud contract the pre-rewrite (substring str start ...)
      ;; gave: an out-of-range start is a caller error, not a silent #f or a
      ;; glibc-clamped spurious match. The bound is the character length, as the
      ;; old substring used and the sibling iterators (regexp-fold /
      ;; regexp-for-each) still enforce.
      (when (or (< start 0) (> start (string-length str)))
        (error 'regexp-search "start index out of range" start))
      (regexp-search/bytes crx str buf start)))

  (define (regexp-search? re str . maybe-start)
    (and (apply regexp-search re str maybe-start) #t))

  (define (regexp-match re str)
    (let ((m (regexp-search re str 0)))
      (and m (= 0 (match:start m 0)) m)))

  ;; ======================================================================
  ;; Substitution
  ;; ======================================================================

  (define (regexp-substitute port match . items)
    (let* ((str (regexp-match-type-string match))
           (range (lambda (item)
                    (cond
                      ((integer? item)
                       (let ((s (match:start match item))
                             (e (match:end match item)))
                         (if s (cons s e) (cons 0 0))))
                      ((eq? 'pre item)
                       (cons 0 (match:start match 0)))
                      ((eq? 'post item)
                       (cons (match:end match 0) (string-length str)))
                      (else (error 'regexp-substitute "illegal item" item))))))
      (if port
          (for-each
            (lambda (item)
              (cond
                ((string? item) (display item port))
                ((procedure? item) (display (item match) port))
                (else
                  (let ((r (range item)))
                    (display (substring str (car r) (cdr r)) port)))))
            items)
          (apply string-append
                 (map (lambda (item)
                        (cond
                          ((string? item) item)
                          ((procedure? item) (item match))
                          (else
                            (let ((r (range item)))
                              (substring str (car r) (cdr r))))))
                      items)))))

  ;; Global substitution accumulates left-to-right into a single output-string
  ;; port in one O(M+N) pass (M matches over an N-char subject). A 'post item
  ;; denotes "the rest of the substitution": when it is the template's final
  ;; item -- the ordinary shape -- its continuation is in tail position and
  ;; drives the enclosing loop, so the walk uses a bounded stack with no
  ;; per-match reverse/append. Only a 'post that is followed by further template
  ;; items (which no ordinary caller writes) recurses, and then solely to keep
  ;; the exact left-to-right order the earlier list-building code produced.
  (define (regexp-substitute/global port re str . items)
    (finalize-regexes!)
    (let* ((crx (coerce-regexp re))
           (str-len (string-length str))
           (buf (string->utf8 str))
           (out (open-output-string)))
      ;; Emit the global substitution of the subject from START into OUT.
      (define (substitute-from start0)
        (let loop ((start start0))
          (let ((match (and (<= start str-len)
                            (regexp-search/bytes crx str buf start))))
            (if (not match)
                ;; No match: emit the untouched tail (nothing past the end).
                (when (<= start str-len)
                  (display (substring str start str-len) out))
                (let* ((m-start (match:start match 0))
                       (m-end (match:end match 0))
                       (empty? (= m-start m-end))
                       (next-start (if empty? (+ m-end 1) m-end)))
                  (let emit ((its items))
                    (cond
                      ;; Template exhausted with no 'post encountered: scsh stops
                      ;; here -- only this first match is emitted, no tail, no
                      ;; further matches.
                      ((null? its) (if #f #f))
                      ((eq? 'post (car its))
                       ;; For an empty match, emit the single skipped char before
                       ;; continuing past it.
                       (when (and empty? (< m-end str-len))
                         (display (string (string-ref str m-end)) out))
                       (if (null? (cdr its))
                           (loop next-start)              ; tail: bounded stack
                           (begin                          ; rare: items after 'post
                             (substitute-from next-start)
                             (emit (cdr its)))))
                      (else
                       (let ((item (car its)))
                         (cond
                           ((string? item) (display item out))
                           ((integer? item)
                            (display (or (match:substring match item) "") out))
                           ((eq? 'pre item)
                            (display (substring str start m-start) out))
                           ((procedure? item) (display (item match) out))
                           (else
                            (error 'regexp-substitute/global "illegal item" item))))
                       (emit (cdr its))))))))))
      (substitute-from 0)
      (let ((result (get-output-string out)))
        (if port
            (display result port)
            result))))

  ;; ======================================================================
  ;; Fold / iteration
  ;; ======================================================================

  (define (regexp-fold re kons knil s . rest)
    (finalize-regexes!)
    (let* ((crx (coerce-regexp re))
           (finish (if (and (pair? rest) (car rest))
                       (car rest)
                       (lambda (i x) x)))
           (start (if (and (pair? rest) (pair? (cdr rest)))
                      (cadr rest)
                      0))
           (buf (string->utf8 s)))
      (when (> start (string-length s))
        (error 'regexp-fold "start index exceeds string length" start))
      (let loop ((i start) (val knil))
        (let ((m (regexp-search/bytes crx s buf i)))
          (if m
              (let ((next-i (match:end m 0)))
                (when (= next-i (match:start m 0))
                  (error 'regexp-fold
                         "zero-length match would cause infinite loop"
                         s i))
                (loop next-i (kons i m val)))
              (finish i val))))))

  (define (regexp-for-each re proc s . maybe-start)
    (finalize-regexes!)
    (let* ((crx (coerce-regexp re))
           (start (if (null? maybe-start) 0 (car maybe-start)))
           (buf (string->utf8 s)))
      (when (> start (string-length s))
        (error 'regexp-for-each "start index exceeds string length" start))
      (let loop ((i start))
        (let ((m (regexp-search/bytes crx s buf i)))
          (when m
            (let ((next-i (match:end m 0)))
              (when (= (match:start m 0) next-i)
                (error 'regexp-for-each
                       "zero-length match would cause infinite loop"
                       s i))
              (proc m)
              (loop next-i)))))))

  ;; ======================================================================
  ;; regexp-fold-right
  ;; ======================================================================
  ;; Port of scsh/rx/re-fold.scm regexp-fold-right.
  ;; Folds right-to-left over matches.
  ;; kons: (match next-non-match-start value) -> value
  ;; finish: (first-match-start value) -> value

  (define (regexp-fold-right re kons knil s . rest)
    (finalize-regexes!)
    (let* ((crx (coerce-regexp re))
           (finish (if (and (pair? rest) (car rest))
                       (car rest)
                       (lambda (i x) x)))
           (start (if (and (pair? rest) (pair? (cdr rest)))
                      (cadr rest)
                      0))
           (buf (string->utf8 s)))
      (when (> start (string-length s))
        (error 'regexp-fold-right "start index exceeds string length" start))
      (cond
       ((regexp-search/bytes crx s buf start) =>
        (lambda (m)
          (finish (match:start m 0)
                  (let recur ((last-m m))
                    (cond
                     ((regexp-search/bytes crx s buf (match:end last-m 0)) =>
                      (lambda (m)
                        (let ((i (match:start m 0)))
                          (when (= i (match:end m 0))
                            (error 'regexp-fold-right
                                   "zero-length match would cause infinite loop"
                                   s i))
                          (kons last-m i (recur m)))))
                     (else (kons last-m (string-length s) knil)))))))
       (else (finish (string-length s) knil)))))


  ) ; end library
