;; test-dot-locking.ss -- Tests for (hafod dot-locking)
;; Run with: scheme --libdirs .:src --script test/test-dot-locking.ss
;; Copyright (c) 2026 Dominic Pearson.

(import (test runner) (hafod dot-locking) (hafod temp-file)
        (only (hafod fileinfo) file-exists? delete-file)
        (chezscheme))

(test-begin "Dot-Locking")

;; obtain-dot-lock creates a .lock file
(let ([f (create-temp-file)])
  (test-assert "obtain-dot-lock returns #t"
    (eq? #t (obtain-dot-lock f)))
  (test-assert ".lock file exists after obtain"
    (file-exists? (string-append f ".lock")))
  (test-assert "release-dot-lock returns #t"
    (release-dot-lock f))
  (test-assert ".lock file gone after release"
    (not (file-exists? (string-append f ".lock"))))
  ;; cleanup
  (guard (e [#t #t]) (delete-file f)))

;; release-dot-lock on unlocked file returns #f
(test-assert "release-dot-lock on unlocked file returns #f"
  (not (release-dot-lock "/tmp/nonexistent-dot-lock-test-hafod")))

;; break-dot-lock removes lock
(let ([f (create-temp-file)])
  (obtain-dot-lock f)
  (test-assert "break-dot-lock removes lock"
    (begin (break-dot-lock f)
           (not (file-exists? (string-append f ".lock")))))
  (guard (e [#t #t]) (delete-file f)))

;; break-dot-lock on non-locked file doesn't error
(test-assert "break-dot-lock on non-locked file succeeds"
  (begin (break-dot-lock "/tmp/nonexistent-dot-lock-test-hafod-2") #t))

;; with-dot-lock acquires and releases
(let ([f (create-temp-file)])
  (test-assert "with-dot-lock runs body and returns value"
    (eqv? 42
      (with-dot-lock f
        (and (file-exists? (string-append f ".lock"))
             42))))
  (test-assert "lock released after with-dot-lock"
    (not (file-exists? (string-append f ".lock"))))
  (guard (e [#t #t]) (delete-file f)))

;; with-dot-lock releases on exception
(let ([f (create-temp-file)])
  (test-assert "with-dot-lock releases on exception"
    (begin
      (guard (e [#t #t])
        (with-dot-lock f
          (error 'test "deliberate error")))
      (not (file-exists? (string-append f ".lock")))))
  (guard (e [#t #t]) (delete-file f)))

;; obtain-dot-lock with retry-number 0 on already-locked file returns #f
(let ([f (create-temp-file)])
  (obtain-dot-lock f)
  (test-assert "obtain-dot-lock returns #f when locked and retries exhausted"
    (not (obtain-dot-lock f 1 0 #f)))
  (release-dot-lock f)
  (guard (e [#t #t]) (delete-file f)))

;; with-dot-lock* procedural version works
(let ([f (create-temp-file)])
  (test-assert "with-dot-lock* runs thunk and returns value"
    (eqv? 99
      (with-dot-lock* f (lambda () 99))))
  (test-assert "lock released after with-dot-lock*"
    (not (file-exists? (string-append f ".lock"))))
  (guard (e [#t #t]) (delete-file f)))

;; Re-locking after release works
(let ([f (create-temp-file)])
  (obtain-dot-lock f)
  (release-dot-lock f)
  (test-assert "re-locking after release works"
    (eq? #t (obtain-dot-lock f)))
  (release-dot-lock f)
  (guard (e [#t #t]) (delete-file f)))

(test-end)
