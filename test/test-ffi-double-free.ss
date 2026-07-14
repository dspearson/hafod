;;; test/test-ffi-double-free.ss -- Proof that the posix-spawnp* failure path
;;; releases each foreign block exactly once and does not abort the allocator.
;;;
;;; posix-spawnp* frees a pid buffer, the argument vector and (when file actions
;;; are supplied) the spawn file-actions block on its way out. The failure branch
;;; frees them inline and then raises; the raise unwinds into the guard handler,
;;; which would free again. A latched, idempotent release collapses those two
;;; calls to a single real free, so each block is released once however the
;;; wrapper leaves.
;;;
;;; This runs OUT-OF-PROCESS, in a forked child, for two reasons. First, a
;;; genuine double-free of an opaque heap block aborts the whole process on a
;;; strict allocator, so an in-process assertion would take the runner down with
;;; it. Second -- and this is the subtle part -- a double-free is SILENTLY
;;; SURVIVABLE on some allocators (a destroy of the freed file-actions block
;;; corrupts the very metadata a double-free check reads), so a clean-exit
;;; assertion alone cannot portably tell one free from two. The child therefore
;;; counts real releases through an observer and reports the count in its exit
;;; code: it exits 0 only when the release ran exactly once. With the latch the
;;; count is one and the child exits cleanly; remove the latch and the count is
;;; two (or, where the allocator is strict, the child aborts) -- either way the
;;; parent sees a non-clean exit and this proof goes RED.
;;;
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (chezscheme)
        (only (hafod internal posix-core) spawn-fault spawn-release-observer)
        (only (hafod posix)
              posix-spawnp posix-fork posix-_exit posix-waitpid
              status:exit-val status:term-sig))

(test-begin "ffi-double-free")

;; Drive the forced failure branch in a child, with a file action supplied so the
;; file-actions block is live and its destroy+free is on the exercised path. The
;; observer counts each real release; the child exits 0 only if it ran exactly
;; once. The parent asserts a clean exit -- exit value 0 and no terminating
;; signal -- so both a count of two (survivable double-free) and an allocator
;; abort (strict double-free) fail the assertion.
(test-assert "the spawn failure path releases each foreign block exactly once; the forked child confirms it"
  (let ([child (posix-fork)])
    (if (zero? child)
        (let ([releases 0])
          (parameterize ([spawn-release-observer (lambda () (set! releases (+ releases 1)))]
                         [spawn-fault (lambda () #t)])
            (guard (e [#t #f])
              (posix-spawnp "/bin/true" '("true") (list (list 'close 3)))))
          (posix-_exit (if (= releases 1) 0 1)))
        (let-values ([(wpid status) (posix-waitpid child 0)])
          (and (eqv? 0 (status:exit-val status))
               (not (status:term-sig status)))))))

(test-end)
