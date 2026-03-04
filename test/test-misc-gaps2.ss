#!chezscheme
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod port-collect)
        (hafod process-state)
        (hafod time)
        (hafod compat)
        (except (chezscheme) time vector-append open-input-file open-output-file getenv
                make-date date?
                date-second date-minute date-hour date-day
                date-month date-year date-week-day date-year-day
                date-zone-offset date-zone-name date-dst?
                date-and-time current-date))

(test-begin "Misc API Gaps - Task 2")

;; ========== port-collect.ss gaps ==========

(test-equal "make-char-port-filter upcases characters"
  "HELLO"
  (let ([filter (make-char-port-filter
                  (lambda (c) (write-char (char-upcase c))))])
    (let ([out (open-output-string)]
          [in (open-input-string "hello")])
      (parameterize ([current-input-port in]
                     [current-output-port out])
        (filter))
      (get-output-string out))))

(test-equal "make-string-port-filter reverses lines"
  "olleh\ndlrow\n"
  (let ([filter (make-string-port-filter
                  (lambda (s) (list->string (reverse (string->list s)))))])
    (let ([out (open-output-string)]
          [in (open-input-string "hello\nworld\n")])
      (parameterize ([current-input-port in]
                     [current-output-port out])
        (filter))
      (get-output-string out))))

(test-equal "make-string-port-filter with #f return skips line"
  "kept\n"
  (let ([filter (make-string-port-filter
                  (lambda (s) (if (string=? s "skip") #f s)))])
    (let ([out (open-output-string)]
          [in (open-input-string "skip\nkept\n")])
      (parameterize ([current-input-port in]
                     [current-output-port out])
        (filter))
      (get-output-string out))))

;; ========== process-state.ss gaps ==========

(test-assert "user-login-name returns a non-empty string"
  (let ([name (user-login-name)])
    (and (string? name) (> (string-length name) 0))))

(test-assert "process-times returns 4 numeric values"
  (let-values ([(ut st cut cst) (process-times)])
    (and (number? ut) (number? st) (number? cut) (number? cst))))

(test-assert "cpu-ticks/sec returns a positive integer"
  (let ([ticks (cpu-ticks/sec)])
    (and (integer? ticks) (> ticks 0))))

;; ========== time.ss gaps ==========

(test-assert "time+ticks returns epoch seconds and ticks"
  (let-values ([(secs ticks) (time+ticks)])
    (and (> secs 1000000000)
         (>= ticks 0))))

(test-equal "ticks/sec returns 1000000 (microsecond resolution)"
  1000000
  (ticks/sec))

(test-end)
