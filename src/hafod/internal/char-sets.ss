;;; (hafod internal char-sets) -- Minimal SRFI-14 subset for hafod
;;; Provides char-set as a predicate wrapper, membership testing, coercion,
;;; algebra (complement, union, intersection, difference), and named POSIX sets.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal char-sets)
  (export char-set char-set-contains? char-set? x->char-set
          char-set:whitespace char-set:newline
          ;; Algebra operations
          char-set-complement char-set-union char-set-intersection
          char-set-difference
          ;; Enumeration and comparison (scan Latin-1 range 0-255)
          char-set-fold char-set-size char-set->list char-set=
          char-set-empty? char-set-full?
          char-set-copy char-set-adjoin char-set-adjoin!
          char-set-delete
          string->char-set
          ;; Full / empty named sets
          char-set:full char-set:empty
          ;; Named POSIX character classes
          char-set:letter char-set:digit char-set:letter+digit
          char-set:lower-case char-set:upper-case
          char-set:punctuation char-set:graphic char-set:printing
          char-set:control char-set:hex-digit char-set:blank
          char-set:ascii char-set:any)
  (import (chezscheme))

  ;; Char-set record: a membership predicate plus an optional 256-byte bitset.
  ;; When bits is a bytevector the set is finite within Latin-1 and membership is an
  ;; O(1) bytevector load; when bits is #f the pred closure answers (this keeps the
  ;; Unicode-wide named sets and every algebra result exact above Latin-1).  The
  ;; case-lambda protocol lets the 1-arg (pred) callers stay untouched.
  (define-record-type char-set-type
    (fields pred bits)
    (protocol (lambda (new)
                (case-lambda
                  ((pred)      (new pred #f))       ; closure set -- unchanged callers
                  ((pred bits) (new pred bits)))))  ; finite set -- carries a bitset
    (nongenerative hafod-char-set-type-v2))

  ;; Public predicate (re-export from record definition)
  (define (char-set? x)
    (char-set-type? x))

  ;; ======================================================================
  ;; Finite-set construction (bitset-backed when provably within Latin-1)
  ;; ======================================================================

  ;; Build a bitset-backed set from a list of chars when every member is within
  ;; Latin-1 (code point < 256); otherwise keep a memv closure so an astral member
  ;; is preserved (a 256-byte bitset could not represent it).
  (define (chars->finite-set chars)
    (if (for-all (lambda (c) (fx<? (char->integer c) 256)) chars)
        (let ((bv (make-bytevector 256 0)))
          (for-each (lambda (c) (bytevector-u8-set! bv (char->integer c) 1)) chars)
          (make-char-set-type #f bv))            ; pred is #f -- never consulted
        (make-char-set-type (lambda (c) (and (memv c chars) #t)))))

  ;; Precompute a bitset from a predicate KNOWN to be a subset of [0,256) by
  ;; scanning 0-255 through it, so the bitset is equivalent to it by construction.
  (define (predicate->finite-set pred)
    (let ((bv (make-bytevector 256 0)))
      (do ((i 0 (fx+ i 1))) ((fx=? i 256))
        (when (pred (integer->char i)) (bytevector-u8-set! bv i 1)))
      (make-char-set-type #f bv)))

  ;; Constructor: (char-set #\a #\b #\c) => char-set containing a, b, c
  (define (char-set . chars)
    (chars->finite-set chars))

  ;; Membership test.  A bitset-backed set answers with a guarded bytevector load --
  ;; the (fx<? cp 256) guard makes any code point >=256 a miss and keeps the result a
  ;; proper #t/#f, so downstream eq? comparisons (char-set=) stay valid.  Otherwise
  ;; the predicate closure answers.
  (define (char-set-contains? cs c)
    (let ((bits (char-set-type-bits cs)))
      (if bits
          (let ((cp (char->integer c)))
            (and (fx<? cp 256) (fx=? 1 (bytevector-u8-ref bits cp))))
          ((char-set-type-pred cs) c))))

  ;; Pre-defined char-sets
  (define char-set:whitespace
    (make-char-set-type char-whitespace?))

  (define char-set:newline
    (predicate->finite-set (lambda (c) (char=? c #\newline))))

  ;; Coerce string/char/char-set to char-set
  (define (x->char-set x)
    (cond
      ((char-set-type? x) x)
      ((char? x) (char-set x))
      ((string? x) (chars->finite-set (string->list x)))
      (else (error 'x->char-set "cannot coerce to char-set" x))))

  ;; ======================================================================
  ;; Enumeration operations (scan Latin-1 range 0-255)
  ;; ======================================================================

  ;; The maximum code point we enumerate.  Latin-1 keeps it fast.
  (define %latin1-max 256)

  ;; char-set-fold kons knil cs => fold over all chars in cs
  (define (char-set-fold kons knil cs)
    (let loop ((i 0) (acc knil))
      (if (>= i %latin1-max) acc
          (let ((c (integer->char i)))
            (if (char-set-contains? cs c)
                (loop (+ i 1) (kons c acc))
                (loop (+ i 1) acc))))))

  ;; char-set-size cs => number of chars in cs (within Latin-1)
  (define (char-set-size cs)
    (char-set-fold (lambda (c n) (+ n 1)) 0 cs))

  ;; char-set->list cs => list of chars in cs
  (define (char-set->list cs)
    (char-set-fold (lambda (c lst) (cons c lst)) '() cs))

  ;; char-set= cs1 cs2 => #t if they contain the same chars (within Latin-1)
  (define (char-set= cs1 cs2)
    (let loop ((i 0))
      (if (>= i %latin1-max) #t
          (let ((c (integer->char i)))
            (if (eq? (char-set-contains? cs1 c) (char-set-contains? cs2 c))
                (loop (+ i 1))
                #f)))))

  ;; char-set-empty? cs => #t if cs contains no chars
  (define (char-set-empty? cs)
    (= 0 (char-set-size cs)))

  ;; char-set-full? cs => #t if cs contains all Latin-1 chars
  (define (char-set-full? cs)
    (= %latin1-max (char-set-size cs)))

  ;; char-set-copy returns a set with same membership (identity for predicate sets)
  (define (char-set-copy cs) cs)

  ;; char-set-adjoin cs char ... => new set with chars added
  (define (char-set-adjoin cs . chars)
    (make-char-set-type
     (lambda (c)
       (or (char-set-contains? cs c)
           (and (memv c chars) #t)))))

  ;; char-set-adjoin! -- linear-update variant (functional in predicate impl)
  (define (char-set-adjoin! cs . chars)
    (apply char-set-adjoin cs chars))

  ;; char-set-delete cs char ... => new set with chars removed
  (define (char-set-delete cs . chars)
    (make-char-set-type
     (lambda (c)
       (and (char-set-contains? cs c)
            (not (memv c chars))))))

  ;; string->char-set str [base-cs] => char-set of chars in str, unioned with base-cs.
  ;; The union is bitset-backed only when base-cs is itself bitset-backed AND every
  ;; char in str is within Latin-1: base's bitset is copied and the string's chars are
  ;; OR-ed in.  Otherwise (a predicate base, or an astral char in str) the original
  ;; union-with-base closure is preserved unchanged.
  (define string->char-set
    (case-lambda
      ((str)
       (chars->finite-set (string->list str)))
      ((str base-cs)
       (let ((base-bits (char-set-type-bits base-cs)))
         (if (and base-bits
                  (for-all (lambda (c) (fx<? (char->integer c) 256)) (string->list str)))
             (let ((bv (bytevector-copy base-bits)))
               (for-each (lambda (c) (bytevector-u8-set! bv (char->integer c) 1))
                         (string->list str))
               (make-char-set-type #f bv))
             (make-char-set-type
              (lambda (c)
                (or (char-set-contains? base-cs c)
                    (let lp ((i 0))
                      (and (< i (string-length str))
                           (or (char=? c (string-ref str i))
                               (lp (+ i 1)))))))))))))

  ;; Full and empty named sets
  (define char-set:full
    (make-char-set-type (lambda (c) #t)))

  (define char-set:empty
    (make-char-set-type (lambda (c) #f)))

  ;; ======================================================================
  ;; Char-set algebra
  ;; ======================================================================

  ;; Complement: everything NOT in cs
  (define (char-set-complement cs)
    (make-char-set-type (lambda (c) (not (char-set-contains? cs c)))))

  ;; Union: in ANY of the given sets
  (define (char-set-union . css)
    (make-char-set-type
     (lambda (c)
       (let loop ((css css))
         (and (pair? css)
              (or (char-set-contains? (car css) c)
                  (loop (cdr css))))))))

  ;; Intersection: in ALL of the given sets
  (define (char-set-intersection . css)
    (if (null? css)
        char-set:any
        (make-char-set-type
         (lambda (c)
           (let loop ((css css))
             (or (null? css)
                 (and (char-set-contains? (car css) c)
                      (loop (cdr css)))))))))

  ;; Difference: in first set but NOT in any of the others
  (define (char-set-difference cs . css)
    (make-char-set-type
     (lambda (c)
       (and (char-set-contains? cs c)
            (let loop ((css css))
              (or (null? css)
                  (and (not (char-set-contains? (car css) c))
                       (loop (cdr css)))))))))

  ;; ======================================================================
  ;; Named POSIX character classes
  ;; ======================================================================

  (define char-set:letter
    (make-char-set-type char-alphabetic?))

  (define char-set:digit
    (predicate->finite-set
     (lambda (c) (and (char>=? c #\0) (char<=? c #\9)))))

  (define char-set:letter+digit
    (make-char-set-type
     (lambda (c) (or (char-alphabetic? c)
                     (and (char>=? c #\0) (char<=? c #\9))))))

  (define char-set:lower-case
    (make-char-set-type char-lower-case?))

  (define char-set:upper-case
    (make-char-set-type char-upper-case?))

  (define char-set:punctuation
    (make-char-set-type
     (lambda (c)
       (let ((n (char->integer c)))
         (and (>= n 33) (<= n 126)
              (not (char-alphabetic? c))
              (not (and (char>=? c #\0) (char<=? c #\9))))))))

  (define char-set:graphic
    (make-char-set-type
     (lambda (c)
       (let ((n (char->integer c)))
         (and (>= n 33) (<= n 126))))))

  (define char-set:printing
    (make-char-set-type
     (lambda (c)
       (let ((n (char->integer c)))
         (and (>= n 32) (<= n 126))))))

  (define char-set:control
    (make-char-set-type
     (lambda (c)
       (let ((n (char->integer c)))
         (or (< n 32) (= n 127))))))

  (define char-set:hex-digit
    (predicate->finite-set
     (lambda (c)
       (or (and (char>=? c #\0) (char<=? c #\9))
           (and (char>=? c #\a) (char<=? c #\f))
           (and (char>=? c #\A) (char<=? c #\F))))))

  (define char-set:blank
    (predicate->finite-set
     (lambda (c) (or (char=? c #\space) (char=? c #\tab)))))

  (define char-set:ascii
    (predicate->finite-set
     (lambda (c) (< (char->integer c) 128))))

  ;; Matches any character (used by char-set-intersection with no args)
  (define char-set:any
    (make-char-set-type (lambda (c) #t)))

  ) ; end library
