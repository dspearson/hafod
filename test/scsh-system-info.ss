;;; Ported from scsh/test/test-packages.scm (system-parameter-tests)
;;; Tests for uname and system-name (section 3.7 of scsh manual)
;;; Original: inline in test-packages.scm. Ported to hafod test runner.
;;;
;;; Deepened toward scsh conformance: rather than re-checking the per-field
;;; non-emptiness the companion system suite already owns, this suite locks the
;;; uname record's contract (its predicate and the type of all five accessors),
;;; the documented equivalence between system-name and the node name, and the
;;; purity of repeated (uname) calls -- cross-procedure invariants a regression
;;; in the record decode or the node-name derivation would break.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod))

(test-begin "scsh-system-info")

(let ((u (uname)))
  ;; The record predicate accepts a genuine (uname) result and rejects a
  ;; non-record value -- the negative case the companion suite omits.
  (test-assert "uname-info? recognises a uname record"
    (uname-info? u))
  (test-assert "uname-info? rejects a non-record value"
    (not (uname-info? 5)))

  ;; All five documented fields return strings. Asserting the type contract as
  ;; a single universal quantification locks the record shape without
  ;; duplicating the per-field non-emptiness checks kept elsewhere.
  (test-assert "all five uname accessors return strings"
    (for-all string?
             (list (uname:os-name u) (uname:node-name u) (uname:release u)
                   (uname:version u) (uname:machine u))))

  ;; scsh documents the host node name as the value of system-name; hafod
  ;; exports both, so they must agree. system-name is a distinct export that
  ;; re-derives the node name, not a mere alias -- a drift between the two
  ;; would redden here.
  (test-assert "system-name equals the uname node-name"
    (equal? (system-name) (uname:node-name u))))

;; (uname) is pure: two independent calls agree on a field. A stale cache or a
;; mutated record would break the agreement.
(test-assert "uname is stable across two calls"
  (equal? (uname:machine (uname)) (uname:machine (uname))))

;; The kernel name is one of the platforms hafod targets. Assert membership,
;; never an exact string, so the check stays portable across Linux, Darwin and
;; FreeBSD hosts.
(test-assert "os-name is a recognised kernel"
  (member (uname:os-name (uname)) '("Linux" "Darwin" "FreeBSD")))

(test-end)
