;;; test/test-env-hashtable.ss -- proof that the O(1) getenv index and the O(n)
;;; sync-env-to-os! diff are byte-identical to the linear-scan originals, and
;;; that the diff is measurably faster than the quadratic (assoc key alist) it
;;; replaces.
;;;
;;; Four witnesses, in the test-charset-bitset / test-poll-vacuity ethos:
;;;
;;;   A. getenv == assoc.  Across an add / overwrite / delete / wholesale
;;;      alist->env (with a duplicate key) / with-total-env* sequence, for every
;;;      watched key (getenv k) equals (cond ((assoc k (env->alist)) => cdr)
;;;      (else #f)).  This proves the index never drifts from the alist, the
;;;      first occurrence winning on duplicate keys.
;;;
;;;   B. env->alist byte-identity.  The same mutation sequence is replayed
;;;      against a reference alist evolved with the module's own alist-update /
;;;      alist-delete, and (equal? (env->alist) reference) is asserted after each
;;;      step -- so ordering AND first-wins duplicate resolution stay verbatim
;;;      (the child envp is built from this order, so it may not shift a byte).
;;;
;;;   C. sync diff equivalence.  Over ~3000 wanted vars + ~200 stale OS-only
;;;      vars, the shipped env-keys-to-unset yields the identical unset-set (by
;;;      key, in order) as a naive (filter (assoc ...)) reference.
;;;
;;;   D. timing witness.  Both diffs are timed with the in-tree (real-time)
;;;      idiom; the membership diff must clear fast < (max 1 (quotient naive 4)).
;;;      RED on the quadratic path (env-keys-to-unset unbound, then assoc-based),
;;;      GREEN once the hashtable diff lands.  The two millisecond figures are
;;;      printed as the recorded before/after for the summary.
;;;
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (except (chezscheme) getenv)
        (only (hafod environment)
              getenv setenv env->alist alist->env with-total-env*
              alist-update alist-delete env-keys-to-unset))

(test-begin "env-hashtable")

;; Wall-clock elapsed milliseconds for a thunk (the in-tree (real-time) idiom,
;; as used by test-charset-bitset.ss / test/poll.ss).
(define (elapsed-ms thunk)
  (let ((t0 (real-time)))
    (thunk)
    (- (real-time) t0)))

;; =============================================================================
;; A -- getenv agrees with the assoc-over-(env->alist) oracle at every step
;; =============================================================================
;; The oracle is the exact pre-index getenv: the frontmost alist entry, or #f.

(define (oracle k)
  (let ((al (env->alist)))
    (cond ((assoc k al) => cdr) (else #f))))

(define (agree? ks)
  (for-all (lambda (k) (equal? (getenv k) (oracle k))) ks))

;; Keys interrogated after each mutation: a freshly-added key, an overwritten
;; one, a deleted one, a duplicated key, a never-present key, two pre-existing
;; real vars, and a block of bulk keys -- so the oracle is checked over many
;; variables at once, not a handful.
(define watch-keys
  (append (list "EQV_ADD" "EQV_OVER" "EQV_DEL" "EQV_DUP" "EQV_MISSING" "PATH" "HOME")
          (map (lambda (i) (string-append "EQV_BULK_" (number->string i)))
               (iota 50))))

(test-assert
  "getenv == (assoc _ (env->alist)) after add / overwrite / delete / wholesale / with-total-env*"
  (let ((ok #t))
    (define (step!) (set! ok (and ok (agree? watch-keys))))
    (step!)                                              ; baseline (real env)
    ;; add many
    (for-each (lambda (i)
                (setenv (string-append "EQV_BULK_" (number->string i))
                        (string-append "v" (number->string i))))
              (iota 50))
    (setenv "EQV_ADD" "1") (step!)
    ;; overwrite the same key twice
    (setenv "EQV_ADD" "2") (step!)
    (setenv "EQV_OVER" "first") (setenv "EQV_OVER" "second") (step!)
    ;; add then delete
    (setenv "EQV_DEL" "x") (step!)
    (setenv "EQV_DEL" #f)  (step!)
    ;; wholesale replace carrying a DUPLICATE key -- the frontmost must win
    (alist->env (list (cons "EQV_DUP" "front") (cons "EQV_ADD" "kept")
                      (cons "EQV_DUP" "back")  (cons "PATH" "/synthetic")))
    (step!)
    ;; the replaced env inside a with-total-env* body must also satisfy the oracle
    (with-total-env* (list (cons "EQV_DUP" "inner-front")
                           (cons "EQV_DUP" "inner-back")
                           (cons "HOME" "/inner"))
      (lambda () (step!)))
    (step!)                                              ; after restore
    ok))

;; =============================================================================
;; B -- env->alist stays byte-identical (order + first-wins) to a reference
;;      alist evolved with the module's own alist-update / alist-delete
;; =============================================================================

(test-assert
  "env->alist is byte-identical to an alist-update/alist-delete reference across the same ops"
  (let ((ref '()) (ok #t))
    (define (check!) (set! ok (and ok (equal? (env->alist) ref))))
    (define (do-set! k v) (setenv k v) (set! ref (alist-update k v ref)) (check!))
    (define (do-del! k)   (setenv k #f) (set! ref (alist-delete k ref)) (check!))
    (define (do-total! al) (alist->env al) (set! ref al) (check!))
    ;; a known baseline (verbatim wholesale replace)
    (do-total! (list (cons "B_K1" "1") (cons "B_K2" "2") (cons "B_K3" "3")))
    ;; an add conses to the front
    (do-set! "B_NEW" "n")
    ;; overwriting an existing key moves it to the front, single occurrence
    (do-set! "B_K2" "22")
    ;; a delete removes it
    (do-del! "B_K1")
    ;; a wholesale replace carrying a DUPLICATE key is kept verbatim
    (do-total! (list (cons "DUP" "front") (cons "B_K3" "3") (cons "DUP" "back")))
    ;; a setenv on the duplicated key collapses it to one front entry, exactly as
    ;; alist-update (cons + alist-delete-all) does -- proving dedup is identical
    (do-set! "DUP" "merged")
    ok))

;; =============================================================================
;; C/D -- the sync-env-to-os! diff: equivalence to the naive assoc reference,
;;         and a measured (real-time) before/after on ~3000 synthetic vars
;; =============================================================================
;; env-keys-to-unset is pure (it touches no OS state), so it can be driven on
;; large synthetic inputs and compared against the naive reference off the live
;; environment -- the shipped O(n) path is exactly what is timed.

(define want-n 3000)
(define stale-n 200)

;; ~3000 wanted vars ...
(define want-alist
  (map (lambda (i) (cons (string-append "WANT" (number->string i))
                         (string-append "val" (number->string i))))
       (iota want-n)))

;; ... plus ~200 stale vars that are in the OS but NOT wanted (must be unset).
(define stale-alist
  (map (lambda (i) (cons (string-append "STALE" (number->string i))
                         (string-append "s" (number->string i))))
       (iota stale-n)))

;; A plausible OS environment: every wanted var plus the stale extras.
(define os-alist (append want-alist stale-alist))

;; The naive "before": one (assoc key want) membership test per OS var -- O(n*m).
(define (naive-unset os al)
  (filter (lambda (op) (not (assoc (car op) al))) os))

;; C -- identical unset-set (by key, in os order).
(test-assert
  "env-keys-to-unset yields the identical unset-set (by key) as the naive assoc diff"
  (equal? (map car (naive-unset os-alist want-alist))
          (map car (env-keys-to-unset os-alist want-alist))))

;; D -- timing witness.  Repeat each diff a few times so the quadratic "before"
;; lands solidly in measurable milliseconds; the membership diff must clear a 4x
;; margin (the probed win is far larger).
(define reps 5)
(define naive-result #f)
(define fast-result  #f)

(define naive-ms
  (elapsed-ms (lambda ()
                (let loop ((i 0))
                  (when (< i reps)
                    (set! naive-result (naive-unset os-alist want-alist))
                    (loop (+ i 1)))))))

(define fast-ms
  (elapsed-ms (lambda ()
                (let loop ((i 0))
                  (when (< i reps)
                    (set! fast-result (env-keys-to-unset os-alist want-alist))
                    (loop (+ i 1)))))))

;; Emit the measured pair unconditionally -- this is the recorded before/after.
(display "WITNESS sync-env-to-os! diff: naive-ms=") (display naive-ms)
(display " fast-ms=") (display fast-ms)
(display " reps=") (display reps)
(display " want=") (display want-n) (display " stale=") (display stale-n)
(display " unset=") (display (length fast-result))
(newline)

;; Non-vacuity: the two timed diffs computed the SAME work (same unset-set), so
;; the comparison is real and not an artefact of one path doing less.
(test-assert
  "the timed fast diff agrees with the timed naive diff (non-vacuous timing)"
  (equal? (map car naive-result) (map car fast-result)))

;; THE witness: RED on the quadratic path, GREEN once the membership diff lands.
;; The (max 1 ...) floor keeps the assertion sound at millisecond resolution.
(test-assert
  "the O(n) membership diff is more than 4x faster than the naive O(n*m) assoc diff"
  (< fast-ms (max 1 (quotient naive-ms 4))))

(test-end)
