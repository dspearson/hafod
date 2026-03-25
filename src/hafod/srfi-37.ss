#!chezscheme
;;; (hafod srfi-37) -- SRFI-37: args-fold: a program argument processor
;;; Reference: https://srfi.schemers.org/srfi-37/srfi-37.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-37)
  (export args-fold option option-names option-required-arg?
          option-optional-arg? option-processor)
  (import (chezscheme))

  (define-record-type option-type
    (fields names required-arg? optional-arg? processor))

  (define (option names required-arg? optional-arg? processor)
    (make-option-type names required-arg? optional-arg? processor))

  (define (option-names opt) (option-type-names opt))
  (define (option-required-arg? opt) (option-type-required-arg? opt))
  (define (option-optional-arg? opt) (option-type-optional-arg? opt))
  (define (option-processor opt) (option-type-processor opt))

  (define (find-option name options)
    (let loop ((opts options))
      (cond
        ((null? opts) #f)
        ((member name (option-names (car opts))) (car opts))
        (else (loop (cdr opts))))))

  (define (args-fold args options unrecognized-option-proc operand-proc . seeds)
    (define (call-proc proc . all-args)
      (call-with-values (lambda () (apply proc all-args)) list))

    (let loop ((args args) (seeds seeds))
      (if (null? args)
          (apply values seeds)
          (let ((arg (car args))
                (rest (cdr args)))
            (cond
              ;; End of options marker
              ((string=? arg "--")
               (let operand-loop ((oargs rest) (seeds seeds))
                 (if (null? oargs)
                     (apply values seeds)
                     (operand-loop
                      (cdr oargs)
                      (apply call-proc operand-proc (car oargs) seeds)))))

              ;; Long option: --name or --name=arg
              ((and (>= (string-length arg) 3)
                    (char=? (string-ref arg 0) #\-)
                    (char=? (string-ref arg 1) #\-))
               (let* ((equals-pos (str-index arg #\= 2))
                      (name (if equals-pos
                                (substring arg 2 equals-pos)
                                (substring arg 2 (string-length arg))))
                      (opt-arg (if equals-pos
                                   (substring arg (+ equals-pos 1) (string-length arg))
                                   #f))
                      (opt (find-option name options)))
                 (cond
                   ((not opt)
                    (loop rest
                          (apply call-proc unrecognized-option-proc
                                 (option (list name) #f #f #f) name opt-arg seeds)))
                   ((and (option-required-arg? opt) (not opt-arg) (pair? rest))
                    (loop (cdr rest)
                          (apply call-proc (option-processor opt) opt name (car rest) seeds)))
                   (else
                    (loop rest
                          (apply call-proc (option-processor opt) opt name opt-arg seeds))))))

              ;; Short options: -x or -xArg or -xyz (combined)
              ((and (>= (string-length arg) 2)
                    (char=? (string-ref arg 0) #\-)
                    (not (char=? (string-ref arg 1) #\-)))
               (let short-loop ((i 1) (seeds seeds) (rest rest))
                 (if (>= i (string-length arg))
                     (loop rest seeds)
                     (let* ((ch (string-ref arg i))
                            (opt (find-option ch options)))
                       (cond
                         ((not opt)
                          (short-loop (+ i 1)
                            (apply call-proc unrecognized-option-proc
                                   (option (list ch) #f #f #f) ch #f seeds)
                            rest))
                         ((option-required-arg? opt)
                          (if (< (+ i 1) (string-length arg))
                              ;; Rest of arg is the argument
                              (loop rest
                                    (apply call-proc (option-processor opt) opt ch
                                           (substring arg (+ i 1) (string-length arg))
                                           seeds))
                              ;; Next arg is the argument
                              (if (pair? rest)
                                  (loop (cdr rest)
                                        (apply call-proc (option-processor opt) opt ch
                                               (car rest) seeds))
                                  (loop rest
                                        (apply call-proc (option-processor opt) opt ch
                                               #f seeds)))))
                         (else
                          (short-loop (+ i 1)
                            (apply call-proc (option-processor opt) opt ch #f seeds)
                            rest)))))))

              ;; Operand
              (else
               (loop rest
                     (apply call-proc operand-proc arg seeds))))))))

  (define (str-index str ch start)
    (let ((n (string-length str)))
      (let loop ((i start))
        (cond
          ((= i n) #f)
          ((char=? (string-ref str i) ch) i)
          (else (loop (+ i 1))))))))
