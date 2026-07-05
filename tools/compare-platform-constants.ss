;; tools/compare-platform-constants.ss -- Cross-check the platform constants against an independent C probe.
;;
;; Reads the generated (hafod internal platform-constants) library as data and the
;; probe's bare "NAME VALUE" output, then compares every constant as an exact
;; integer.  It names the exact constant that disagrees rather than printing a
;; whole-file diff, and runs a set-difference in both directions so a constant
;; present on only one side can never slip through unnoticed.
;;
;; Parsing fails closed: a probe line that is not "NAME <integer>", or a define
;; whose value is not an exact integer, is a fatal error, never a silent skip -- a
;; skipped line would be a constant escaping verification.
;;
;; Run: scheme --script tools/compare-platform-constants.ss <constants.ss> <probe-output>
;;   <constants.ss>  the constants library to read as data
;;                   (defaults to src/hafod/internal/platform-constants.ss)
;;   <probe-output>  a file of the probe's "NAME VALUE" lines (required)
(import (chezscheme))

;; ----------------------------------------------------------------------
;; Command-line arguments
;; ----------------------------------------------------------------------
;; The constants path is optional and comes first; the probe-output path is
;; required.  Every caller passes both, in that order, so a temporary copy of the
;; constants file can be checked without touching the tracked one.

(define arguments (cdr (command-line)))

(define default-constants-path "src/hafod/internal/platform-constants.ss")

(define constants-path
  (if (>= (length arguments) 2)
      (car arguments)
      default-constants-path))

(define probe-path
  (cond
    [(>= (length arguments) 2) (cadr arguments)]
    [(= (length arguments) 1) (car arguments)]
    [else
     (fprintf (current-error-port)
              "usage: scheme --script tools/compare-platform-constants.ss <constants.ss> <probe-output>~n")
     (exit 1)]))

;; ----------------------------------------------------------------------
;; Reader 1: the constants library, read as data
;; ----------------------------------------------------------------------
;; Read the whole (library (hafod internal platform-constants) ...) form and walk
;; its body, collecting each (define NAME value) as a (NAME . value) pair.  The
;; (export ...) and (import ...) forms are ignored.  A define whose value is not an
;; exact integer is fatal (fail closed): every constant must be a comparable integer.

(define (read-constants path)
  (call-with-input-file path
    (lambda (port)
      (let ([form (read port)])
        (unless (and (pair? form) (eq? (car form) 'library))
          (errorf 'compare-platform-constants
                  "~a does not hold a (library ...) form" path))
        (let walk ([body (cddr form)] [pairs '()])
          (cond
            [(null? body) (reverse pairs)]
            [(and (pair? (car body)) (eq? (caar body) 'define))
             (let ([name (cadr (car body))]
                   [value (caddr (car body))])
               (unless (and (integer? value) (exact? value))
                 (errorf 'compare-platform-constants
                         "constant ~a is not an exact integer: ~s" name value))
               (walk (cdr body) (cons (cons name value) pairs)))]
            [else (walk (cdr body) pairs)]))))))

;; ----------------------------------------------------------------------
;; Reader 2: the probe output, read fail-closed into a NAME -> value table
;; ----------------------------------------------------------------------
;; Each line must be exactly "NAME VALUE": split on spaces into two fields, parse
;; the value as a base-ten exact integer, and store it under the name.  Anything
;; else -- the wrong field count, a value that is not an exact integer, or a
;; repeated name -- is fatal, so no malformed or duplicated line passes unnoticed.

(define (split-on-space str)
  (let ([len (string-length str)])
    (let scan ([i 0] [start 0] [fields '()])
      (cond
        [(= i len) (reverse (cons (substring str start i) fields))]
        [(char=? (string-ref str i) #\space)
         (scan (+ i 1) (+ i 1) (cons (substring str start i) fields))]
        [else (scan (+ i 1) start fields)]))))

(define (read-probe path)
  (let ([table (make-hashtable string-hash string=?)])
    (call-with-input-file path
      (lambda (port)
        (let read-lines ()
          (let ([line (get-line port)])
            (unless (eof-object? line)
              (let ([fields (split-on-space line)])
                (unless (= (length fields) 2)
                  (errorf 'compare-platform-constants
                          "probe line is not \"NAME VALUE\": ~s" line))
                (let ([name (car fields)]
                      [value (string->number (cadr fields) 10)])
                  (unless (and value (integer? value) (exact? value))
                    (errorf 'compare-platform-constants
                            "probe value is not an exact integer: ~s" line))
                  (when (hashtable-contains? table name)
                    (errorf 'compare-platform-constants
                            "probe repeats the constant ~a" name))
                  (hashtable-set! table name value)))
              (read-lines))))))
    table))

;; ----------------------------------------------------------------------
;; The skip-list
;; ----------------------------------------------------------------------
;; Constants deliberately left out of the cross-check would be named here -- a
;; future derived value with no direct sizeof/offsetof/#define witness.  It is
;; empty today: every constant is probeable, so all of them are compared.  Keep
;; the mechanism so an omission is always an explicit, reviewed entry.

(define skip-list '())

(define (skipped? name)
  (and (member name skip-list) #t))

;; ----------------------------------------------------------------------
;; The comparison
;; ----------------------------------------------------------------------
;; Two directions, so a constant on only one side cannot escape:
;;   1. every define in the library, against the probe table -- a value that
;;      disagrees is a MISMATCH, a name the probe never emitted is MISSING-IN-PROBE;
;;   2. every name the probe emitted, against the library -- one the library never
;;      defined is MISSING-IN-SS.
;; Values are compared with = on the exact integers from read / string->number, so
;; a wide unsigned mask or a -1 sentinel compares correctly without truncation.
;; Each result names the exact constant rather than a whole-file diff.

(define (compare constants probe)
  (let ([constant-names (make-hashtable string-hash string=?)])
    (for-each
      (lambda (pair)
        (hashtable-set! constant-names (symbol->string (car pair)) #t))
      constants)
    (let ([forward
            (fold-left
              (lambda (records pair)
                (let ([name (symbol->string (car pair))]
                      [ss-value (cdr pair)])
                  (if (skipped? name)
                      records
                      (let ([probe-value (hashtable-ref probe name #f)])
                        (cond
                          [(not probe-value)
                           (cons (cons 'missing-in-probe
                                       (format "MISSING-IN-PROBE ~a: ss=~a" name ss-value))
                                 records)]
                          [(not (= ss-value probe-value))
                           (cons (cons 'mismatch
                                       (format "MISMATCH ~a: ss=~a probe=~a"
                                               name ss-value probe-value))
                                 records)]
                          [else records])))))
              '()
              constants)]
          [backward
            (fold-left
              (lambda (records name)
                (if (or (skipped? name) (hashtable-contains? constant-names name))
                    records
                    (cons (cons 'missing-in-ss
                                (format "MISSING-IN-SS ~a: probe=~a"
                                        name (hashtable-ref probe name #f)))
                          records)))
              '()
              (list-sort string<? (vector->list (hashtable-keys probe))))])
      (append (reverse forward) (reverse backward)))))

(define (count-of category records)
  (fold-left (lambda (n record) (if (eq? (car record) category) (+ n 1) n))
             0 records))

;; ----------------------------------------------------------------------
;; Run it
;; ----------------------------------------------------------------------
;; Print every disagreement (naming the constant) and exit non-zero with a one-line
;; summary; otherwise report the dynamic count that agreed and exit zero.

(define constants (read-constants constants-path))
(define probe (read-probe probe-path))
(define records (compare constants probe))
(define total (length constants))
(define skipped (length skip-list))

(if (null? records)
    (printf "all ~a constants agree with the independent C probe (~a skipped)~n"
            (- total skipped) skipped)
    (begin
      (for-each (lambda (record) (printf "~a~n" (cdr record))) records)
      (fprintf (current-error-port)
               "~a mismatch(es), ~a missing-in-probe, ~a missing-in-ss across ~a constants~n"
               (count-of 'mismatch records)
               (count-of 'missing-in-probe records)
               (count-of 'missing-in-ss records)
               total)
      (exit 1)))
