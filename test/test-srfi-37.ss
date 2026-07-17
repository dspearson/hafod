;;; test-srfi-37.ss -- Conformance tests for the args library (SRFI 37).
;;;
;;; SRFI 37's args-fold walks a command line, dispatching short options (-x),
;;; long options (--name), the --name=value form, the -- operands terminator
;;; and bare operands, threading a seed through the option and operand
;;; processors. This suite builds a small option set and asserts the parse
;;; order across all of those forms, plus the option accessors. The audit
;;; dispositioned the parser conformant.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-37) (chezscheme))

(test-begin "srfi-37")

;; ===========================================================================
;; Section A -- an option's accessors report what it was built with
;; ===========================================================================
;; The recording processors below cons a tagged event onto the single seed;
;; the names list carries both the short character and the long string so the
;; one option matches -v and --verbose alike.

(define (verbose-proc opt name arg seed) (cons (list 'verbose name arg) seed))
(define (file-proc opt name arg seed) (cons (list 'file name arg) seed))
(define (unrecognised-proc opt name arg seed) (cons (list 'unknown name arg) seed))
(define (operand-proc arg seed) (cons (list 'operand arg) seed))

(define verbose-opt (option '(#\v "verbose") #f #f verbose-proc))
(define file-opt (option '(#\f "file") #t #f file-proc))
(define opts (list verbose-opt file-opt))

(test-equal "option-names carries both the short character and the long name"
            '(#\v "verbose") (option-names verbose-opt))
(test-assert "option-required-arg? is true for the file option"
             (option-required-arg? file-opt))
(test-assert "option-optional-arg? is false for the file option"
             (not (option-optional-arg? file-opt)))
(test-assert "option-processor returns the stored procedure"
             (procedure? (option-processor file-opt)))

;; ===========================================================================
;; Section B -- a full parse over short, long, =value, -- and operands
;; ===========================================================================
;; "first.txt" is a leading operand; -v a short flag; --file=out.txt a long
;; option with an =-joined value; -f in.txt a short option taking the next
;; argument; -- ends option processing; operand1 is the trailing operand.

(define parsed
  (reverse
   (args-fold
    '("first.txt" "-v" "--file=out.txt" "-f" "in.txt" "--" "operand1")
    opts unrecognised-proc operand-proc '())))

(test-equal "args-fold threads short/long/=value/--/operand parses in order"
            '((operand "first.txt")
              (verbose #\v #f)
              (file "file" "out.txt")
              (file #\f "in.txt")
              (operand "operand1"))
            parsed)

;; ===========================================================================
;; Section C -- an unrecognised option reaches the unrecognised processor
;; ===========================================================================

(test-equal "an unknown short option is routed to the unrecognised processor"
            '((unknown #\x #f))
            (reverse (args-fold '("-x") opts unrecognised-proc operand-proc '())))

(test-end)
