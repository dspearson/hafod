(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod time) (hafod posix) (hafod compat)
        (except (chezscheme) time vector-append
                make-date date?
                date-second date-minute date-hour date-day
                date-month date-year date-week-day date-year-day
                date-zone-offset date-zone-name date-dst?
                date-and-time current-date))

(test-begin "Date/Time")

;; ---- Date record ----

(test-assert "make-date creates a date record"
  (date? (make-date 0 30 14 15 5 2026)))

(let ([d (make-date 30 45 10 15 5 2026 "UTC" 0 #f 1 135)])
  (test-equal "date:seconds" 30 (date:seconds d))
  (test-equal "date:minute" 45 (date:minute d))
  (test-equal "date:hour" 10 (date:hour d))
  (test-equal "date:month-day" 15 (date:month-day d))
  (test-equal "date:month" 5 (date:month d))
  (test-equal "date:year" 2026 (date:year d))
  (test-equal "date:tz-name" "UTC" (date:tz-name d))
  (test-equal "date:tz-secs" 0 (date:tz-secs d))
  (test-assert "date:summer? is #f" (not (date:summer? d)))
  (test-equal "date:week-day" 1 (date:week-day d))
  (test-equal "date:year-day" 135 (date:year-day d)))

;; Short constructor
(test-assert "make-date with 6 args works"
  (date? (make-date 0 0 0 1 0 2020)))

;; ---- time ----

(test-assert "time returns an integer"
  (integer? (time)))

(test-assert "time returns positive epoch seconds"
  (> (time) 1000000000))  ;; After 2001

;; ---- date ----

(test-assert "date returns a date record"
  (date? (date)))

(test-assert "date:year is reasonable"
  (>= (date:year (date)) 2020))

(test-assert "date:month is 0-11"
  (let ([m (date:month (date))])
    (and (>= m 0) (<= m 11))))

(test-assert "date:hour is 0-23"
  (let ([h (date:hour (date))])
    (and (>= h 0) (<= h 23))))

;; date with specific epoch
(let ([d (date 0)])  ;; Epoch = Jan 1, 1970
  (test-equal "date of epoch 0: year is 1970" 1970 (date:year d)))

;; date with UTC zone
(let ([d (date 0 0)])  ;; Epoch 0, UTC
  (test-equal "date of epoch 0 UTC: year is 1970" 1970 (date:year d))
  (test-equal "date of epoch 0 UTC: month is 0 (January)" 0 (date:month d))
  (test-equal "date of epoch 0 UTC: month-day is 1" 1 (date:month-day d))
  (test-equal "date of epoch 0 UTC: hour is 0" 0 (date:hour d)))

;; ---- date->string ----

(test-assert "date->string returns a string"
  (string? (date->string (date))))

(test-assert "date->string contains year"
  (let ([s (date->string (date))])
    ;; Should contain the current year as a substring
    (let ([year-str (number->string (date:year (date)))])
      (let ([slen (string-length s)]
            [ylen (string-length year-str)])
        (let loop ([i 0])
          (cond
            [(> (+ i ylen) slen) #f]
            [(string=? (substring s i (+ i ylen)) year-str) #t]
            [else (loop (+ i 1))]))))))

;; ---- format-date ----

(let ([d (date 0 0)])  ;; Epoch 0, UTC
  (test-equal "format-date ~Y-~m-~d for epoch 0 UTC"
    "1970-01-01"
    (format-date "~Y-~m-~d" d)))

(let ([d (date 0 0)])  ;; Epoch 0, UTC
  (test-equal "format-date ~H:~M:~S for epoch 0 UTC"
    "00:00:00"
    (format-date "~H:~M:~S" d)))

;; ---- Round-trip: time -> date -> time ----

(let* ([t1 (time)]
       [d (date t1)]
       [t2 (time d)])
  ;; Should be approximately equal (within a few seconds due to DST/mktime quirks)
  (test-assert "time->date->time round-trip within 2 seconds"
    (<= (abs (- t1 t2)) 2)))

(test-end)
