(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod glob) (hafod posix) (hafod compat)
        (except (chezscheme) vector-append open-input-file open-output-file getenv))

;; String helper for test convenience
(define (string-suffix? suffix str)
  (let ([slen (string-length suffix)]
        [len (string-length str)])
    (and (>= len slen)
         (string=? (substring str (- len slen) len) suffix))))

(define (string-split str ch)
  (let ([len (string-length str)])
    (let loop ([i 0] [start 0] [acc '()])
      (cond
        [(= i len)
         (reverse (cons (substring str start len) acc))]
        [(char=? (string-ref str i) ch)
         (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]
        [else
         (loop (+ i 1) start acc)]))))

(test-begin "Glob")

;; Setup: create test files for glob testing
(let ([dir "/tmp/hafod-glob-test"])
  ;; Clean up any previous run
  (guard (e [#t #f])
    (for-each (lambda (f) (posix-unlink (string-append dir "/" f)))
              '("alpha.ss" "beta.ss" "gamma.txt" ".hidden"))
    (posix-rmdir dir))

  (posix-mkdir dir #o755)
  ;; Create test files by creating and renaming
  (for-each (lambda (name)
              (let ([path (string-append dir "/" name)])
                (receive (tmp-path fd) (posix-mkstemp (string-append dir "/tmp-XXXXXX"))
                  (posix-close fd)
                  (posix-rename tmp-path path))))
            '("alpha.ss" "beta.ss" "gamma.txt" ".hidden"))

  ;; Test: glob with * wildcard
  (let ([results (glob (string-append dir "/*.ss"))])
    (test-assert "glob *.ss finds .ss files"
      (>= (length results) 2))
    (test-assert "glob *.ss results contain alpha.ss"
      (exists (lambda (f) (string-suffix? "alpha.ss" f)) results))
    (test-assert "glob *.ss results contain beta.ss"
      (exists (lambda (f) (string-suffix? "beta.ss" f)) results))
    (test-assert "glob *.ss does not contain .txt"
      (not (exists (lambda (f) (string-suffix? ".txt" f)) results))))

  ;; Test: glob with ? wildcard
  (let ([results (glob (string-append dir "/????.ss"))])
    ;; "beta" is 4 chars + ".ss" so "beta.ss" matches ????.ss
    (test-assert "glob with ? wildcard works"
      (exists (lambda (f) (string-suffix? "beta.ss" f)) results)))

  ;; Test: glob *.txt
  (let ([results (glob (string-append dir "/*.txt"))])
    (test-assert "glob *.txt finds txt files"
      (= (length results) 1))
    (test-assert "glob *.txt result is gamma.txt"
      (exists (lambda (f) (string-suffix? "gamma.txt" f)) results)))

  ;; Test: glob * doesn't match dot-files
  (let ([results (glob (string-append dir "/*"))])
    (test-assert "glob * excludes dot-files"
      (not (exists (lambda (f) (string-suffix? ".hidden" f)) results))))

  ;; Test: glob .* matches dot-files
  (let ([results (glob (string-append dir "/.*"))])
    (test-assert "glob .* includes dot-files"
      (exists (lambda (f) (string-suffix? ".hidden" f)) results))
    (test-assert "glob .* excludes . and .."
      (not (exists (lambda (f)
                      (let ([base (let ([parts (string-split f #\/)])
                                    (if (pair? parts) (car (reverse parts)) ""))])
                        (or (string=? base ".") (string=? base ".."))))
                    results))))

  ;; Test: no matches returns empty list
  (test-equal "glob with no matches returns '()"
    '()
    (glob (string-append dir "/*.xyz-nonexistent")))

  ;; Cleanup
  (for-each (lambda (name) (posix-unlink (string-append dir "/" name)))
            '("alpha.ss" "beta.ss" "gamma.txt" ".hidden"))
  (posix-rmdir dir))

;; Test: glob-quote
(test-equal "glob-quote escapes *"
  "\\*" (glob-quote "*"))

(test-equal "glob-quote escapes ?"
  "\\?" (glob-quote "?"))

(test-equal "glob-quote escapes brackets"
  "\\[test\\]" (glob-quote "[test]"))

(test-equal "glob-quote escapes braces"
  "\\{a,b\\}" (glob-quote "{a,b}"))

(test-equal "glob-quote leaves normal chars alone"
  "hello" (glob-quote "hello"))

(test-equal "glob-quote escapes backslash"
  "\\\\path" (glob-quote "\\path"))

;; Test: brace expansion
(let ([dir "/tmp/hafod-glob-brace"])
  (guard (e [#t #f])
    (posix-unlink (string-append dir "/a.txt"))
    (posix-unlink (string-append dir "/b.txt"))
    (posix-rmdir dir))
  (posix-mkdir dir #o755)
  (for-each (lambda (name)
              (let ([path (string-append dir "/" name)])
                (receive (tmp-path fd) (posix-mkstemp (string-append dir "/tmp-XXXXXX"))
                  (posix-close fd)
                  (posix-rename tmp-path path))))
            '("a.txt" "b.txt"))

  (let ([results (glob (string-append dir "/{a,b}.txt"))])
    (test-assert "brace expansion works"
      (= (length results) 2)))

  (posix-unlink (string-append dir "/a.txt"))
  (posix-unlink (string-append dir "/b.txt"))
  (posix-rmdir dir))

;; Test: absolute path glob
(test-assert "glob with absolute path works"
  (pair? (glob "/tmp/*")))

;; Test: character class patterns [abc], [a-z], [^abc], [!abc]
(let ([dir "/tmp/hafod-glob-charclass"])
  (guard (e [#t #f])
    (for-each (lambda (f) (posix-unlink (string-append dir "/" f)))
              '("a.txt" "b.txt" "c.txt" "d.txt" "1.txt"))
    (posix-rmdir dir))

  (posix-mkdir dir #o755)
  (for-each (lambda (name)
              (let ([path (string-append dir "/" name)])
                (receive (tmp-path fd) (posix-mkstemp (string-append dir "/tmp-XXXXXX"))
                  (posix-close fd)
                  (posix-rename tmp-path path))))
            '("a.txt" "b.txt" "c.txt" "d.txt" "1.txt"))

  ;; [abc].txt matches a, b, c but not d or 1
  (let ([results (glob (string-append dir "/[abc].txt"))])
    (test-assert "[abc] matches a.txt"
      (exists (lambda (f) (string-suffix? "a.txt" f)) results))
    (test-assert "[abc] matches b.txt"
      (exists (lambda (f) (string-suffix? "b.txt" f)) results))
    (test-assert "[abc] matches c.txt"
      (exists (lambda (f) (string-suffix? "c.txt" f)) results))
    (test-assert "[abc] does not match d.txt"
      (not (exists (lambda (f) (string-suffix? "d.txt" f)) results)))
    (test-assert "[abc] does not match 1.txt"
      (not (exists (lambda (f) (string-suffix? "1.txt" f)) results))))

  ;; [a-c].txt matches a, b, c but not d
  (let ([results (glob (string-append dir "/[a-c].txt"))])
    (test-assert "[a-c] matches a.txt"
      (exists (lambda (f) (string-suffix? "a.txt" f)) results))
    (test-assert "[a-c] matches c.txt"
      (exists (lambda (f) (string-suffix? "c.txt" f)) results))
    (test-assert "[a-c] does not match d.txt"
      (not (exists (lambda (f) (string-suffix? "d.txt" f)) results))))

  ;; [!abc].txt matches d and 1 but not a, b, c (POSIX standard negation)
  (let ([results (glob (string-append dir "/[!abc].txt"))])
    (test-assert "[!abc] matches d.txt"
      (exists (lambda (f) (string-suffix? "d.txt" f)) results))
    (test-assert "[!abc] does not match b.txt"
      (not (exists (lambda (f) (string-suffix? "b.txt" f)) results))))

  ;; Cleanup
  (for-each (lambda (name) (posix-unlink (string-append dir "/" name)))
            '("a.txt" "b.txt" "c.txt" "d.txt" "1.txt"))
  (posix-rmdir dir))

(test-end)
