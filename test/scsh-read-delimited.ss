;;; scsh-read-delimited.ss -- Ported scsh read-delimited-strings tests
;;; Translated from scsh/test/read-delimited-strings.scm
;;; Original author: Christoph Hetz
;;; Ported to hafod test runner format.
;;; Run with: scheme --libdirs .:src --script test/scsh-read-delimited.ss

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod) (hafod internal char-sets))

(test-begin "scsh-read-delimited")

;;; ===== read-line tests =====

;; 1. read-line-test: basic read-line from string port, 3 lines
(test-assert "read-line-test"
  (let ([p (open-input-string "zeile 1\nzeile 2\nzeile 3")])
    (and (equal? "zeile 1" (read-line p))
         (equal? "zeile 2" (read-line p))
         (equal? "zeile 3" (read-line p)))))

;; 2. read-line-trim-test: explicit 'trim mode (same as default)
(test-assert "read-line-trim-test"
  (let ([p (open-input-string "zeile 1\nzeile 2\nzeile 3")])
    (and (equal? "zeile 1" (read-line p 'trim))
         (equal? "zeile 2" (read-line p 'trim))
         (equal? "zeile 3" (read-line p 'trim)))))

;; 3. read-line-peek-test: 'peek mode -- delimiter left in port
(test-assert "read-line-peek-test"
  (let ([p (open-input-string "zeile 1\nzeile 2\nzeile 3")])
    (and (equal? "zeile 1" (read-line p 'peek))
         (equal? "" (read-line p 'peek))
         (equal? "" (read-line p 'peek)))))

;; 4. read-line-concat-test: 'concat mode -- delimiter appended to result
;;    Input has \004 (Ctrl-D) literally embedded at end.
;;    hafod read-line only uses newline as delimiter, so \004 is data.
(test-assert "read-line-concat-test"
  (let ([p (open-input-string
             (string-append "zeile 1\nzeile 2\nzeile 3" (string (integer->char 4))))])
    (and (equal? "zeile 1\n" (read-line p 'concat))
         (equal? "zeile 2\n" (read-line p 'concat))
         (equal? (string-append "zeile 3" (string (integer->char 4)))
                 (read-line p 'concat)))))

;; 5. read-line-split-test: 'split mode -- returns 2 values (string, delimiter-or-eof)
(test-assert "read-line-split-test"
  (let ([p (open-input-string "zeile 1\nzeile 2\nzeile 3")])
    (and (call-with-values (lambda () (read-line p 'split))
           (lambda (a b) (and (equal? a "zeile 1")
                              (equal? b #\newline))))
         (call-with-values (lambda () (read-line p 'split))
           (lambda (a b) (and (equal? a "zeile 2")
                              (equal? b #\newline))))
         (call-with-values (lambda () (read-line p 'split))
           (lambda (a b) (and (equal? a "zeile 3")
                              (eof-object? b)))))))

;;; ===== read-paragraph tests =====

;; 6. read-paragraph-test: default trim mode, 3 paragraphs
(test-assert "read-paragraph-test"
  (let ([p (open-input-string "zeile 1\nzeile 2\nparagraph 1\n\nzeile 1\nparagraph 2\n    \t\nzeile 1\nparagraph 3\n\n")])
    (and (equal? "zeile 1\nzeile 2\nparagraph 1\n" (read-paragraph p))
         (equal? "zeile 1\nparagraph 2\n" (read-paragraph p))
         (equal? "zeile 1\nparagraph 3\n" (read-paragraph p)))))

;; 7. read-paragraph-trim-test: explicit trim (same as default)
(test-assert "read-paragraph-trim-test"
  (let ([p (open-input-string "zeile 1\nzeile 2\nparagraph 1\n\nzeile 1\nparagraph 2\n    \t\nzeile 1\nparagraph 3\n\n")])
    (and (equal? "zeile 1\nzeile 2\nparagraph 1\n" (read-paragraph p 'trim))
         (equal? "zeile 1\nparagraph 2\n" (read-paragraph p 'trim))
         (equal? "zeile 1\nparagraph 3\n" (read-paragraph p 'trim)))))

;; 8. read-paragraph-concat-test: 'concat mode
(test-assert "read-paragraph-concat-test"
  (let ([p (open-input-string "zeile 1\nzeile 2\nparagraph 1\n\nzeile 1\nparagraph 2\n    \t\nzeile 1\nparagraph 3\n\n")])
    (and (equal? "zeile 1\nzeile 2\nparagraph 1\n\n" (read-paragraph p 'concat))
         (equal? "zeile 1\nparagraph 2\n    \t\n" (read-paragraph p 'concat))
         (equal? "zeile 1\nparagraph 3\n\n" (read-paragraph p 'concat)))))

;; 9. read-paragraph-split-test: 'split mode, returns 2 values
(test-assert "read-paragraph-split-test"
  (let ([p (open-input-string "zeile 1\nzeile 2\nparagraph 1\n\nzeile 1\nparagraph 2\n    \t\nzeile 1\nparagraph 3\n\n")])
    (and (call-with-values (lambda () (read-paragraph p 'split))
           (lambda (a b) (and (equal? a "zeile 1\nzeile 2\nparagraph 1\n")
                              (equal? b "\n"))))
         (call-with-values (lambda () (read-paragraph p 'split))
           (lambda (a b) (and (equal? a "zeile 1\nparagraph 2\n")
                              (equal? b "    \t\n"))))
         (call-with-values (lambda () (read-paragraph p 'split))
           (lambda (a b) (and (equal? a "zeile 1\nparagraph 3\n")
                              (equal? b "\n")))))))

;;; ===== read-delimited with char-set tests =====

;; 10. read-delimited-with-char-set-test: default/trim
(test-assert "read-delimited-with-char-set-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1"
                 (read-delimited (char-set #\a #\b #\:) p))
         (equal? " nix\nzeile 2: x"
                 (read-delimited (char-set #\a #\b #\y) p)))))

;; 11. read-delimited-trim-with-char-set-test
(test-assert "read-delimited-trim-with-char-set-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1"
                 (read-delimited (char-set #\a #\b #\:) p 'trim))
         (equal? " nix\nzeile 2: x"
                 (read-delimited (char-set #\a #\b #\y) p 'trim)))))

;; 12. read-delimited-peek-with-char-set-test
(test-assert "read-delimited-peek-with-char-set-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1"
                 (read-delimited (char-set #\a #\b #\:) p 'peek))
         (equal? ": nix\nzeile 2: x"
                 (read-delimited (char-set #\a #\b #\y) p 'peek)))))

;; 13. read-delimited-concat-with-char-set-test
(test-assert "read-delimited-concat-with-char-set-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1:"
                 (read-delimited (char-set #\a #\b #\:) p 'concat))
         (equal? " nix\nzeile 2: xy"
                 (read-delimited (char-set #\a #\b #\y) p 'concat)))))

;; 14. read-delimited-split-with-char-set-test
(test-assert "read-delimited-split-with-char-set-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (call-with-values
           (lambda () (read-delimited (char-set #\a #\b #\:) p 'split))
           (lambda (a b) (and (equal? "zeile 1" a)
                              (equal? #\: b))))
         (call-with-values
           (lambda () (read-delimited (char-set #\a #\b #\y) p 'split))
           (lambda (a b) (and (equal? " nix\nzeile 2: x" a)
                              (equal? #\y b)))))))

;;; ===== read-delimited with string tests =====

;; 15. read-delimited-with-string-test
(test-assert "read-delimited-with-string-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1"
                 (read-delimited "ab:" p))
         (equal? " nix\nzeile 2: x"
                 (read-delimited "aby" p)))))

;; 16. read-delimited-trim-with-string-test
(test-assert "read-delimited-trim-with-string-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1"
                 (read-delimited "ab:" p 'trim))
         (equal? " nix\nzeile 2: x"
                 (read-delimited "aby" p 'trim)))))

;; 17. read-delimited-peek-with-string-test
(test-assert "read-delimited-peek-with-string-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1"
                 (read-delimited "ab:" p 'peek))
         (equal? ": nix\nzeile 2: x"
                 (read-delimited "aby" p 'peek)))))

;; 18. read-delimited-concat-with-string-test
(test-assert "read-delimited-concat-with-string-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1:"
                 (read-delimited "ab:" p 'concat))
         (equal? " nix\nzeile 2: xy"
                 (read-delimited "aby" p 'concat)))))

;; 19. read-delimited-split-with-string-test
(test-assert "read-delimited-split-with-string-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (call-with-values
           (lambda () (read-delimited "ab:" p 'split))
           (lambda (a b) (and (equal? "zeile 1" a)
                              (equal? #\: b))))
         (call-with-values
           (lambda () (read-delimited "aby" p 'split))
           (lambda (a b) (and (equal? " nix\nzeile 2: x" a)
                              (equal? #\y b)))))))

;;; ===== read-delimited with character tests =====

;; 20. read-delimited-with-character-test
(test-assert "read-delimited-with-character-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1"
                 (read-delimited #\: p))
         (equal? " nix\nzeile 2: x"
                 (read-delimited #\y p)))))

;; 21. read-delimited-trim-with-character-test
(test-assert "read-delimited-trim-with-character-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1"
                 (read-delimited #\: p 'trim))
         (equal? " nix\nzeile 2: x"
                 (read-delimited #\y p 'trim)))))

;; 22. read-delimited-peek-with-character-test
(test-assert "read-delimited-peek-with-character-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1"
                 (read-delimited #\: p 'peek))
         (equal? ": nix\nzeile 2: x"
                 (read-delimited #\y p 'peek)))))

;; 23. read-delimited-concat-with-character-test
(test-assert "read-delimited-concat-with-character-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (equal? "zeile 1:"
                 (read-delimited #\: p 'concat))
         (equal? " nix\nzeile 2: xy"
                 (read-delimited #\y p 'concat)))))

;; 24. read-delimited-split-with-character-test
(test-assert "read-delimited-split-with-character-test"
  (let ([p (open-input-string "zeile 1: nix\nzeile 2: xy\nzeile 3: wieder nix\n")])
    (and (call-with-values
           (lambda () (read-delimited #\: p 'split))
           (lambda (a b) (and (equal? "zeile 1" a)
                              (equal? #\: b))))
         (call-with-values
           (lambda () (read-delimited #\y p 'split))
           (lambda (a b) (and (equal? " nix\nzeile 2: x" a)
                              (equal? #\y b)))))))

;;; ===== skip-char-set tests =====

;; 25. skip-char-set-with-charset-test
(test-assert "skip-char-set-with-charset-test"
  (= 6
     (skip-char-set (char-set #\a #\b #\c)
                    (open-input-string "abccbaxxx"))))

;; 26. skip-char-set-with-string-test
(test-assert "skip-char-set-with-string-test"
  (= 6
     (skip-char-set "abc"
                    (open-input-string "abccbaxxx"))))

;; 27. skip-char-set-with-character-test
(test-assert "skip-char-set-with-character-test"
  (= 6
     (skip-char-set #\a
                    (open-input-string "aaaaaaxxx"))))

(test-end)
