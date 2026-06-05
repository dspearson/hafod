(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod system)
        (hafod posix)
        (chezscheme))

(test-begin "System Utilities")

;; ---- uname ----

(test-assert "uname returns a uname-info record"
  (uname-info? (uname)))

(test-assert "uname:os-name returns a non-empty string"
  (let ([s (uname:os-name (uname))])
    (and (string? s) (> (string-length s) 0))))

(test-assert "uname:os-name is a known OS name"
  (member (uname:os-name (uname)) '("Linux" "Darwin" "FreeBSD")))

(test-assert "uname:node-name returns a string"
  (string? (uname:node-name (uname))))

(test-assert "uname:release returns a non-empty string"
  (let ([s (uname:release (uname))])
    (and (string? s) (> (string-length s) 0))))

(test-assert "uname:version returns a non-empty string"
  (let ([s (uname:version (uname))])
    (and (string? s) (> (string-length s) 0))))

(test-assert "uname:machine returns a non-empty string"
  (let ([s (uname:machine (uname))])
    (and (string? s) (> (string-length s) 0))))

;; ---- errno-error ----

(test-assert "errno-error raises a condition catchable by guard"
  (guard (e [#t #t])
    (errno-error 2 'test "file not found")
    #f))

(test-assert "errno-error raises a condition with posix-error? predicate"
  (guard (e [(posix-error? e) #t]
            [#t #f])
    (errno-error 2 'test "file not found")
    #f))

(test-assert "errno-error condition has correct errno"
  (guard (e [(posix-error? e)
             (= (posix-errno e) 2)]
            [#t #f])
    (errno-error 2 'test "file not found")
    #f))

;; ---- with-errno-handler* ----

(test-assert "with-errno-handler* catches posix errors"
  (with-errno-handler*
    (lambda (errno exn) #t)
    (lambda () (errno-error 13 'test "permission denied") #f)))

(test-assert "with-errno-handler* passes errno to handler"
  (with-errno-handler*
    (lambda (errno exn) (= errno 13))
    (lambda () (errno-error 13 'test "permission denied") #f)))

(test-assert "with-errno-handler* returns thunk result on no error"
  (equal? 42
    (with-errno-handler*
      (lambda (errno exn) #f)
      (lambda () 42))))

;; ---- with-errno-handler syntax ----

(test-assert "with-errno-handler catches matching errno"
  (with-errno-handler
    (((2) #t))
    (errno-error 2 'test "not found")
    #f))

(test-assert "with-errno-handler re-raises unmatched errno"
  (guard (e [(posix-error? e) (= (posix-errno e) 13)]
            [#t #f])
    (with-errno-handler
      (((2) #f))
      (errno-error 13 'test "permission denied")
      #f)))

;; ---- version constants ----

;; Versions are generated from git tags at build time (tools/gen-version.sh),
;; so assert their *shape* rather than a hardcoded literal that would need
;; editing on every release.
(test-assert "hafod-major-version is a non-negative integer"
  (and (integer? hafod-major-version) (>= hafod-major-version 0)))

(test-assert "hafod-minor-version is a non-negative integer"
  (and (integer? hafod-minor-version) (>= hafod-minor-version 0)))

(test-assert "hafod-version-string is \"hafod <version>\""
  (let ([s hafod-version-string])
    (and (string? s)
         (> (string-length s) 6)
         (string=? "hafod " (substring s 0 6)))))

;; scsh-compatible version aliases mirror the hafod values
(test-equal "scsh-major-version alias" hafod-major-version scsh-major-version)
(test-equal "scsh-minor-version alias" hafod-minor-version scsh-minor-version)
(test-equal "scsh-version-string alias" hafod-version-string scsh-version-string)

(test-end)
