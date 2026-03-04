;; test-lib-dirs.ss -- Tests for (hafod lib-dirs)
;; Run with: scheme --libdirs .:src --script test/test-lib-dirs.ss
;; Copyright (c) 2026 Dominic Pearson.

(import (test runner) (hafod lib-dirs) (chezscheme))

(test-begin "Library Search Path")

;; lib-dirs returns a list
(test-assert "lib-dirs returns a list"
  (list? (lib-dirs)))

;; Default dirs include the standard module path
(test-assert "default lib-dirs is non-empty"
  (pair? (lib-dirs)))

(test-assert "default lib-dirs contains strings"
  (andmap string? (lib-dirs)))

;; clear-lib-dirs!
(test-assert "clear-lib-dirs! sets to empty"
  (begin (clear-lib-dirs!)
         (null? (lib-dirs))))

;; reset-lib-dirs! restores defaults
(test-assert "reset-lib-dirs! restores non-empty list"
  (begin (reset-lib-dirs!)
         (pair? (lib-dirs))))

;; lib-dirs-prepend!
(test-assert "lib-dirs-prepend! adds to front"
  (begin
    (clear-lib-dirs!)
    (lib-dirs-prepend! "/aaa")
    (equal? (car (lib-dirs)) "/aaa")))

;; lib-dirs-append!
(test-assert "lib-dirs-append! adds to end"
  (begin
    (lib-dirs-append! "/zzz")
    (let ([dirs (lib-dirs)])
      (equal? (list-ref dirs (- (length dirs) 1)) "/zzz"))))

;; Order after prepend and append
(test-equal "ordering: prepend then append"
  '("/aaa" "/zzz")
  (begin
    (clear-lib-dirs!)
    (lib-dirs-prepend! "/aaa")
    (lib-dirs-append! "/zzz")
    (lib-dirs)))

;; Multiple prepends
(test-equal "multiple prepends stack in LIFO order"
  '("/bbb" "/aaa")
  (begin
    (clear-lib-dirs!)
    (lib-dirs-prepend! "/aaa")
    (lib-dirs-prepend! "/bbb")
    (lib-dirs)))

;; find-library-file returns #f for nonexistent
(test-assert "find-library-file returns #f for nonexistent file"
  (begin
    (clear-lib-dirs!)
    (lib-dirs-prepend! "/tmp/")
    (not (find-library-file "no-such-file-xyz-hafod-test.scm"))))

;; find-library-file finds a real file
(test-assert "find-library-file finds file in lib-dirs"
  (begin
    (clear-lib-dirs!)
    (lib-dirs-prepend! "/tmp/")
    ;; Create a temp file in /tmp
    (let ([p (open-output-file "/tmp/hafod-test-lib-find.tmp" 'replace)])
      (display "test" p)
      (close-output-port p))
    (let ([result (find-library-file "hafod-test-lib-find.tmp")])
      (when (file-exists? "/tmp/hafod-test-lib-find.tmp")
        (delete-file "/tmp/hafod-test-lib-find.tmp"))
      (and result (string? result)))))

;; find-library-file with directory without trailing slash
(test-assert "find-library-file works with dir without trailing slash"
  (begin
    (clear-lib-dirs!)
    (lib-dirs-prepend! "/tmp")
    (let ([p (open-output-file "/tmp/hafod-test-lib-find2.tmp" 'replace)])
      (display "test" p)
      (close-output-port p))
    (let ([result (find-library-file "hafod-test-lib-find2.tmp")])
      (when (file-exists? "/tmp/hafod-test-lib-find2.tmp")
        (delete-file "/tmp/hafod-test-lib-find2.tmp"))
      (and result (string? result)))))

;; find-library-file with empty lib-dirs
(test-assert "find-library-file returns #f with empty lib-dirs"
  (begin
    (clear-lib-dirs!)
    (not (find-library-file "anything.scm"))))

;; Restore for later tests
(reset-lib-dirs!)

(test-end)
