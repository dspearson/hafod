;;; (test poll) -- Wall-clock-bounded polling for suites that wait on an
;;; asynchronous or external event: a forked child reaching a state, a signal
;;; arriving, a file appearing.  Shared, so that no suite re-invents the wait as
;;; an iteration-count spin — a spin bounds the number of attempts, not the time
;;; they are given, and so gives up before a child that has not yet been
;;; scheduled can possibly be observed.
;;;
;;; This library is deliberately NOT named test-*.ss.  The Makefile runs every
;;; test/*.ss it is not told is a harness file as a script, and Chez, handed a
;;; library form via --script, simply defines the library and exits 0 printing
;;; nothing.  A harness library that escaped the Makefile's TEST_HARNESS list
;;; would therefore not fail loudly — it would appear in the run as a silently
;;; passing suite.  It is a source-only library (like test/runner.ss) that
;;; resolves under the suites' --libdirs .:src.
;;;
;;; Copyright (c) 2026, hafod contributors.

(library (test poll)
  (export poll-until)
  (import (chezscheme))

  ;; Test (ready?); while it does not hold and MS milliseconds of wall-clock have not
  ;; yet elapsed, call (step) and retry. Return #t once observed ready, #f on timeout;
  ;; (step) is not called at all if (ready?) already holds on entry. Bounding by
  ;; real time rather than a fixed iteration count is what makes the stopped-child
  ;; cases deterministic: a freshly forked child needs a scheduler slice to run its
  ;; self-SIGSTOP, but a tight 200-iteration spin of a bare waitpid can burn every
  ;; iteration in microseconds — before the stop is even deliverable — and give up
  ;; before it is ever observable. real-time (Chez, milliseconds) bounds the wait by
  ;; the wall clock; each step's waitpid syscall is itself a scheduler yield point.
  (define (poll-until ms step ready?)
    (let ([deadline (+ (real-time) ms)])
      (let loop ()
        (cond [(ready?) #t]
              [(>= (real-time) deadline) #f]
              [else (step) (loop)]))))

  ) ; end library
