;;; Ported from scsh/test/test-packages.scm (system-parameter-tests)
;;; Tests for uname and system-name (section 3.7 of scsh manual)
;;; Original: inline in test-packages.scm. Ported to hafod test runner.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod))

(test-begin "scsh-system-info")

;; uname: verify node-name is non-empty string
(test-assert "uname node-name non-empty"
  (> (string-length (uname:node-name (uname))) 0))

;; system-name equivalent: hafod doesn't export system-name separately,
;; but it's equivalent to uname:node-name per scsh/scheme/system.scm.
;; Verify the node-name is a non-empty string (same semantics).
(test-assert "system-name (via uname:node-name) non-empty"
  (let ((name (uname:node-name (uname))))
    (and (string? name) (> (string-length name) 0))))

(test-end)
