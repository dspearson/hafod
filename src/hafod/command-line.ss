;;; (hafod command-line) -- Command-line argument access
;;; Ported from scsh/scheme/command-line.scm
;;; Copyright (c) 1993 Olin Shivers. R6RS adaptation (c) 2026, hafod contributors.

(library (hafod command-line)
  (export command-line-arguments command-line arg arg* argv set-command-line-args!)
  (import (except (rename (chezscheme) (command-line chez:command-line))
                  command-line-arguments))

  ;; Internal state: use a vector box to avoid R6RS set!-exported-variable restriction.
  ;; Box holds the full argv list (program + args).
  (define %cl-box (vector (chez:command-line)))

  (define (%command-line) (vector-ref %cl-box 0))

  ;; Return args without program name.
  ;; Made a procedure (not a variable) for R6RS export compatibility.
  (define (command-line-arguments)
    (let ((cl (vector-ref %cl-box 0)))
      (if (pair? cl) (cdr cl) '())))

  (define (set-command-line-args! args)
    (vector-set! %cl-box 0 args))

  ;; Return a copy of the full command line (includes program name)
  (define (command-line) (append (%command-line) '()))

  ;; arg* arglist n [default-thunk]
  ;; 1-indexed positional access into arglist.
  (define (arg* arglist n . maybe-default-thunk)
    (let ((oops (lambda () (error 'arg* "argument out of bounds" arglist n))))
      (if (< n 1) (oops)
          (let lp ((al arglist) (n n))
            (if (pair? al)
                (if (= n 1) (car al)
                    (lp (cdr al) (- n 1)))
                (if (and (pair? maybe-default-thunk)
                         (null? (cdr maybe-default-thunk)))
                    ((car maybe-default-thunk))
                    (oops)))))))

  ;; arg arglist n [default]
  ;; Like arg* but takes a default value, not a thunk.
  (define (arg arglist n . maybe-default)
    (if (pair? maybe-default)
        (arg* arglist n (lambda () (car maybe-default)))
        (arg* arglist n)))

  ;; argv n [default]
  ;; 0-indexed access into full command line (n=0 is program name).
  (define (argv n . maybe-default)
    (apply arg (%command-line) (+ n 1) maybe-default))

  ) ; end library
