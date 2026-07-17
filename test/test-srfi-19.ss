;;; test-srfi-19.ss -- Conformance tests for the time library (SRFI 19).
;;;
;;; The time library carried two latent correctness bugs, latent precisely
;;; because no suite exercised it: time-utc->date decoded the wrong civil date
;;; (a sign error in the days-to-calendar arithmetic put epoch 0 at 1969-06-26
;;; instead of 1970-01-01), and date->time-utc subtracted the zone offset a
;;; second time on top of the offset the Julian-day conversion already applied.
;;; The first three sections below reddened on the pre-fix tree and are the
;;; non-vacuous proof of each fix; the later sections give the date surface real
;;; coverage.
;;; Copyright (c) 2026, hafod contributors.

(import (test runner) (hafod srfi-19) (chezscheme))

(test-begin "srfi-19")

;; ===========================================================================
;; Section A -- epoch 0 decodes to 1970-01-01 (the civil-from-days sign fix)
;; ===========================================================================
;; make-time takes (type nanosecond second); epoch 0 is 1970-01-01T00:00:00Z.
;; Pre-fix this decoded to 1969-06-26 -- ten-plus days and half a year adrift.

(define epoch0 (time-utc->date (make-time time-utc 0 0) 0))

(test-equal "epoch 0 decodes to year 1970" 1970 (date-year epoch0))
(test-equal "epoch 0 decodes to month 1" 1 (date-month epoch0))
(test-equal "epoch 0 decodes to day 1" 1 (date-day epoch0))

;; ===========================================================================
;; Section B -- date->time-utc then time-utc->date is the identity in UTC
;; ===========================================================================
;; make-date takes (nanosecond second minute hour day month year zone-offset);
;; at zone 0 the round trip stays in UTC, so every calendar field must survive.
;; Pre-fix 2020-06-15 came back as 2020-06-25.

(define (roundtrip-ymd nanosecond second minute hour day month year)
  (let ((d (time-utc->date
             (date->time-utc
               (make-date nanosecond second minute hour day month year 0))
             0)))
    (list (date-year d) (date-month d) (date-day d))))

(test-equal "1970-01-01 round-trips through UTC unchanged"
            '(1970 1 1) (roundtrip-ymd 0 0 0 12 1 1 1970))
(test-equal "2000-01-01 round-trips through UTC unchanged"
            '(2000 1 1) (roundtrip-ymd 0 0 0 12 1 1 2000))
(test-equal "2020-06-15 round-trips through UTC unchanged (not 2020-06-25)"
            '(2020 6 15) (roundtrip-ymd 0 0 0 12 15 6 2020))
(test-equal "2024-02-29 (leap day) round-trips through UTC unchanged"
            '(2024 2 29) (roundtrip-ymd 0 0 0 12 29 2 2024))
(test-equal "1999-12-31 round-trips through UTC unchanged"
            '(1999 12 31) (roundtrip-ymd 0 0 0 12 31 12 1999))

;; A full round trip including the time-of-day fields for one instant.
(define midday
  (time-utc->date
    (date->time-utc (make-date 0 26 9 15 14 3 2021 0)) 0))
(test-equal "2021-03-14 15:09:26 round-trips its calendar and clock fields"
            '(2021 3 14 15 9 26)
            (list (date-year midday) (date-month midday) (date-day midday)
                  (date-hour midday) (date-minute midday) (date-second midday)))

;; ===========================================================================
;; Section C -- date->time-utc applies the zone offset exactly once
;; ===========================================================================
;; The same wall-clock reading at zone +3600 is one hour EARLIER in absolute
;; UTC than at zone 0; pre-fix the offset was counted twice (a 7200 gap).

(define utc-at-zone-3600
  (time-second (date->time-utc (make-date 0 0 0 0 1 1 2000 3600))))
(define utc-at-zone-0
  (time-second (date->time-utc (make-date 0 0 0 0 1 1 2000 0))))

(test-equal "a +3600 zone instant is exactly 3600 seconds earlier in UTC than the zone-0 wall clock"
            (- utc-at-zone-0 3600) utc-at-zone-3600)

;; ===========================================================================
;; Section D -- week-day for known calendar dates
;; ===========================================================================
;; date-week-day is 0=Sunday .. 6=Saturday. 2000-01-01 was a Saturday,
;; 2000-03-01 a Wednesday (past the leap boundary), 2021-01-01 a Friday.

(test-equal "2000-01-01 is a Saturday" 6
            (date-week-day (make-date 0 0 0 0 1 1 2000 0)))
(test-equal "2000-03-01 is a Wednesday" 3
            (date-week-day (make-date 0 0 0 0 1 3 2000 0)))
(test-equal "2021-01-01 is a Friday" 5
            (date-week-day (make-date 0 0 0 0 1 1 2021 0)))

;; ===========================================================================
;; Section E -- year-day and leap-year handling around the February boundary
;; ===========================================================================
;; year-day is 1-based. 1 March is day 61 in a leap year and day 60 otherwise,
;; so these four dates prove the leap rule (divisible by 4, by 100, by 400).

(test-equal "1 March 2020 is year-day 61 (2020 is a leap year)" 61
            (date-year-day (make-date 0 0 0 0 1 3 2020 0)))
(test-equal "1 March 2021 is year-day 60 (2021 is not a leap year)" 60
            (date-year-day (make-date 0 0 0 0 1 3 2021 0)))
(test-equal "1 March 2000 is year-day 61 (2000 is a leap year: divisible by 400)" 61
            (date-year-day (make-date 0 0 0 0 1 3 2000 0)))
(test-equal "1 March 1900 is year-day 60 (1900 is not a leap year: divisible by 100 not 400)" 60
            (date-year-day (make-date 0 0 0 0 1 3 1900 0)))
(test-equal "31 December 2020 is year-day 366 (a leap year has 366 days)" 366
            (date-year-day (make-date 0 0 0 0 31 12 2020 0)))

;; ===========================================================================
;; Section F -- date->string supported directives
;; ===========================================================================
;; The supported directive set is ~Y ~m ~d ~H ~M ~S and the ~~ literal-tilde
;; escape; padding is zero-fill to width. Any other directive is passed through.

(define ref-date (make-date 0 26 9 15 14 3 2021 0))

(test-equal "date->string default format renders zero-padded ~Y-~m-~d ~H:~M:~S"
            "2021-03-14 15:09:26" (date->string ref-date))
(test-equal "date->string zero-pads single-digit month and day"
            "2000-01-01 00:00:00" (date->string (make-date 0 0 0 0 1 1 2000 0)))
(test-equal "date->string ~~ is a literal tilde" "~" (date->string ref-date "~~"))
(test-equal "date->string honours an individual ~Y directive" "2021"
            (date->string ref-date "~Y"))
(test-equal "date->string honours ~H:~M:~S" "15:09:26"
            (date->string ref-date "~H:~M:~S"))

;; ===========================================================================
;; Section G -- pre-year-0000 (negative-era) civil decode
;; ===========================================================================
;; Dates before 0000-03-01 sit in the negative-z range of the civil-from-days
;; algorithm.  R6RS div already floors for a positive divisor, so the era term
;; must not re-apply a truncation bias (as C++ integer division would), or the
;; decoded day lands one off: 0000-02-28 would round-trip to 0000-02-29.
(let ((rt (time-utc->date (date->time-utc (make-date 0 0 0 0 28 2 0 0)) 0)))
  (test-equal "pre-year-0000 round-trip keeps day 28" 28 (date-day rt))
  (test-equal "pre-year-0000 round-trip keeps month 2" 2 (date-month rt))
  (test-equal "pre-year-0000 round-trip keeps year 0" 0 (date-year rt)))

(test-end)
