#!chezscheme
;; test-runner-teeth.ss -- Assert that (test runner) can actually fail.
;; Run with: scheme --libdirs .:src --script test/test-runner-teeth.ss
;; Copyright (c) 2026 Dominic Pearson.
;;
;; Every other suite's green is worth precisely as much as the runner's ability
;; to go red.  That ability is one line of test/runner.ss -- test-end's
;; (when (> fails 0) (exit 1)) -- and nothing guarded it.  Were it ever to stop
;; exiting non-zero, every suite in the tree would report success regardless of
;; what its assertions actually did, and no build would notice.  This suite is
;; the guard.
;;
;; Why it spawns a child process.  A suite cannot prove the point by containing
;; a failing assertion: it would fail itself.  The failure has to be observed in
;; a child suite, and that child has to be a real interpreter process rather
;; than a fork.  test-end calls exit only on FAILURE; on a clean run it merely
;; RETURNS, so a forked child would fall off the end of its body and carry on
;; executing the parent's remaining code.  Only a separate process has an end to
;; run off.
;;
;; Why both directions are asserted.  A failing suite must exit non-zero AND a
;; passing suite must exit zero.  Either half alone is satisfied by a broken
;; runner: one that always exited non-zero would pass the first, and one that
;; never exited at all would pass the second.  Only the pair pins the behaviour
;; down.
;;
;; Why the interpreter is taken from the environment.  The child must be the
;; same Chez as the parent -- a mismatched one raises an incompatible-fasl error
;; that would surface here as a spurious failure and could mask a real
;; regression.  Both build front ends export SCHEME into every suite's
;; environment naming the interpreter they actually selected, so that is where
;; it is resolved from first, ahead of whatever a bare PATH search turns up.

(import (test runner)
        (except (chezscheme) vector-append exit open-input-file open-output-file
                             truncate-file delete-file rename-file
                             make-date date? getenv alias)
        (hafod)
        (hafod temp-file))

(test-begin "runner-teeth")

;; ============================================================================
;; Helpers
;; ============================================================================

(define (substring? needle haystack)
  (let ([nl (string-length needle)] [hl (string-length haystack)])
    (let loop ([i 0])
      (cond [(> (+ i nl) hl) #f]
            [(string=? needle (substring haystack i (+ i nl))) #t]
            [else (loop (+ i 1))]))))

;; The interpreter the child suites must run under.  $SCHEME is the one make or
;; just actually selected and exported into this suite's environment; prefer it,
;; so the child is the build's own Chez rather than whichever interpreter a
;; manipulated PATH happens to offer first.  exec-path-search takes a name
;; containing a slash as-is (checking it is executable) and searches PATH for a
;; bare one, so the single call covers both forms $SCHEME may hold.  The
;; scheme/chez-scheme fallback mirrors the Makefile's own auto-detection and
;; keeps this suite runnable when invoked by hand.
(define scheme-bin
  (let loop ([candidates (let ([named (getenv "SCHEME")])
                           (if (and named (not (string=? named "")))
                               (list named "scheme" "chez-scheme")
                               (list "scheme" "chez-scheme")))])
    (cond [(null? candidates) #f]
          [(exec-path-search (car candidates) (exec-path-list))]
          [else (loop (cdr candidates))])))

;; Run SOURCE-TEXT as a suite in a child interpreter; return its exit code and
;; everything it printed.
;;
;; Both temp paths come from create-temp-file, i.e. from mkstemp: exclusive
;; creation, an unpredictable name, and $TMPDIR honoured.  The unpredictability
;; is load-bearing rather than tidy -- the source file is one this suite then
;; EXECUTES, so a guessable path would hand anyone else on the host a code
;; execution primitive, to be had by winning the race between the write and the
;; run.
;;
;; The child resolves (test runner) from ./test/runner.ss through --libdirs .:src
;; because those directories are relative to the working directory, which is the
;; repository root; that the child's own source sits under $TMPDIR does not enter
;; into it.  Its stdin is /dev/null, as every suite's is, so nothing can block on
;; a terminal.  Its stderr is left alone: an interpreter that failed to start has
;; more to say than an exit code, and it should reach the build log rather than a
;; file this suite is about to unlink.
;;
;; run yields a RAW wait status -- 256, not 1, for a child that exited 1 -- so
;; decode it with status:exit-val.
(define (run-child-suite source-text)
  (let ([source-path (create-temp-file (string-append (*temp-file-template*) "teeth-src-"))]
        [output-path (create-temp-file (string-append (*temp-file-template*) "teeth-out-"))])
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (let ([port (open-output-file source-path)])
          (display source-text port)
          (close port))
        (let* ([status (run (,scheme-bin "--libdirs" ".:src" "--script" ,source-path)
                            (> ,output-path)
                            (< "/dev/null"))]
               [code (status:exit-val status)]
               [port (open-input-file output-path)]
               [text (get-string-all port)])
          (close port)
          (values code (if (eof-object? text) "" text))))
      (lambda ()
        (guard (e [#t #t]) (posix-unlink source-path))
        (guard (e [#t #t]) (posix-unlink output-path))))))

;; The two child suites.  Deliberately minimal: one assertion apiece, so that
;; what is being proven about the runner is unambiguous.
(define failing-child-source
  (string-append
    "(import (test runner))\n"
    "(test-begin \"teeth-failing-child\")\n"
    "(test-equal \"two unequal integers\" 1 2)\n"
    "(test-end)\n"))

(define passing-child-source
  (string-append
    "(import (test runner))\n"
    "(test-begin \"teeth-passing-child\")\n"
    "(test-assert \"a trivially true assertion\" #t)\n"
    "(test-end)\n"))

;; ============================================================================
;; The teeth
;; ============================================================================

;; Latched by every check below, and re-read after test-end has returned.  See
;; "the verdict" at the foot of the file: this suite may not report its own
;; result through the mechanism it is testing.
(define teeth-intact? #t)

;; Assert, and latch the outcome in the same breath, so that the two can never
;; drift apart.  These delegate to the runner for the report a reader wants --
;; test-equal's expected/actual lines especially -- while keeping an
;; independent record of what was seen.
(define (check-equal msg expected actual)
  (test-equal msg expected actual)
  (unless (equal? expected actual) (set! teeth-intact? #f)))

(define (check-assert msg held)
  (test-assert msg held)
  (unless held (set! teeth-intact? #f)))

(check-assert "the interpreter to run the child suites under resolves"
  (and scheme-bin #t))

;; Guarded on scheme-bin so an unresolved interpreter is exactly one clean
;; assertion failure above, rather than an exec of #f whose incidental exit
;; status could satisfy a check below for the wrong reason.  The suite still
;; reaches test-end and still exits 1; it simply does not lie about why.
(when scheme-bin

  (let-values ([(code output) (run-child-suite failing-child-source)])
    (check-equal "a suite with a failing assertion exits 1" 1 code)
    (check-assert "...and says which assertion failed, and how many"
      (and (substring? "FAIL:" output)
           (substring? "1 failed" output))))

  (let-values ([(code output) (run-child-suite passing-child-source)])
    (check-equal "a suite whose assertions all pass exits 0" 0 code)
    (check-assert "...and reports no failures"
      (and (substring? "0 failed" output)
           (not (substring? "FAIL:" output))))))

(test-end)

;; ============================================================================
;; The verdict -- delivered on this suite's own authority
;; ============================================================================
;;
;; A suite that tests a mechanism may not use that mechanism as its reporting
;; channel.  Every other suite in the tree leaves the verdict to test-end, which
;; exits 1 when it has recorded a failure -- but that exit is the very thing
;; under test here.  Pull it out of the runner and test-end simply RETURNS, so
;; this suite would fall off its end and exit 0: it would have detected the
;; breakage, printed FAIL, and then reported success to make, exactly like every
;; other suite in the tree.  A build would go green with the harness's teeth on
;; the floor, which is the one outcome this file exists to prevent.
;;
;; So test-end returning is ambiguous -- it means EITHER that every assertion
;; above passed, OR that the runner has lost the ability to say otherwise.  The
;; observations were latched as they were made, and are re-read here to tell the
;; two apart.  This exit is not a duplicate of the runner's; it is the one that
;; still works when the runner's does not.
(unless teeth-intact?
  (display "FAIL: the assertion harness has lost its teeth -- a suite can no")
  (newline)
  (display "      longer signal failure by its exit status, so every suite in")
  (newline)
  (display "      the tree would report success regardless of its assertions.")
  (newline)
  (exit 1))
