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

;; ===========================================================================
;; Section H -- UTC <-> TAI leap-second conversions
;; ===========================================================================
;; make-time takes (type nanosecond second).  The embedded historical UTC-TAI
;; table carries the whole-second offset in force at each instant; a TAI==UTC
;; stub returns the naive second unchanged and fails every offset below.

(test-equal "time-utc->time-tai adds the +34 offset in force in 2009"
            1230768034
            (time-second (time-utc->time-tai (make-time time-utc 0 1230768000))))
(test-equal "time-tai->time-utc subtracts the +34 offset back to UTC"
            1230768000
            (time-second (time-tai->time-utc (make-time time-tai 0 1230768034))))
(test-equal "one second before the 1999 boundary keeps the old +31 offset"
            915148799
            (time-second (time-tai->time-utc (make-time time-tai 0 915148830))))
(test-assert "time-tai->time-utc inverts time-utc->time-tai"
             (let ((t (make-time time-utc 0 1230768000)))
               (time=? t (time-tai->time-utc (time-utc->time-tai t)))))
(test-equal "the embedded table carries the 2017 +37 row"
            1483228837
            (time-second (time-utc->time-tai (make-time time-utc 0 1483228800))))
(test-equal "time-utc->time-tai! aliases the non-bang conversion"
            1230768034
            (time-second (time-utc->time-tai! (make-time time-utc 0 1230768000))))
(test-assert "time-tai->time-utc! aliases the non-bang conversion"
             (let ((t (make-time time-utc 0 1230768000)))
               (time=? t (time-tai->time-utc! (time-utc->time-tai! t)))))

;; read-leap-second-table swaps the whole table.  A fixture whose only row is
;; the 1972 baseline drops the 2017 instant's offset to +10.  This runs LAST in
;; Section H: no later section reads the leap-second table, so the swap is inert.
(let* ((tmpdir (or (getenv "TMPDIR") "/tmp"))
       (fixture (string-append tmpdir "/hafod-tai-utc-fixture.dat")))
  (when (file-exists? fixture) (delete-file fixture))
  (call-with-output-file fixture
    (lambda (p)
      (put-string p "1972 JAN 1 =JD 2441317.5 TAI-UTC= 10.0 S + (MJD - 41317.) X 0.0 S\n")))
  (read-leap-second-table fixture)
  (test-equal "read-leap-second-table swaps in a fixture whose sole row is 1972 (+10)"
              1483228810
              (time-second (time-utc->time-tai (make-time time-utc 0 1483228800))))
  (delete-file fixture))

;; ===========================================================================
;; Section I -- Julian-day and modified-Julian-day inverse conversions
;; ===========================================================================
;; JD 2451545 is exactly 2000-01-01T12:00:00 UTC, and MJD 51544 is midnight the
;; same day; feed EXACT integers so the rational arithmetic stays exact.

(test-equal "julian-day->date decodes JD 2451545 to 2000-01-01T12:00:00"
            '(2000 1 1 12 0 0)
            (let ((d (julian-day->date 2451545 0)))
              (list (date-year d) (date-month d) (date-day d)
                    (date-hour d) (date-minute d) (date-second d))))
(test-assert "julian-day->date inverts date->julian-day at JD 2451545"
             (= 2451545 (date->julian-day (julian-day->date 2451545 0))))
(test-equal "modified-julian-day->date decodes MJD 51544 to 2000-01-01T00:00:00"
            '(1 2000 0)
            (let ((d (modified-julian-day->date 51544 0)))
              (list (date-day d) (date-year d) (date-hour d))))

;; ===========================================================================
;; Section J -- the remaining date->string output directives
;; ===========================================================================
;; The dispatch gains the full SRFI 19 directive set with the ~-/~_ pad
;; modifiers.  Weekday/month names, year-day and the ISO week oracle are
;; anchored to facts already pinned by Sections D and E.

(test-equal "ISO ~4 renders the combined date-time with zone"
            "2006-05-04T03:02:01Z"
            (date->string (make-date 0 1 2 3 4 5 2006 0) "~4"))
(test-equal "~I renders midnight as the 12-hour clock's 12"
            "12"
            (date->string (make-date 0 0 0 0 1 9 2018 0) "~I"))
(test-equal "~V gives ISO week 53 for 2020-12-31"
            "53" (date->string (make-date 0 0 0 0 31 12 2020 0) "~V"))
(test-equal "~V gives ISO week 01 for 2021-01-04"
            "01" (date->string (make-date 0 0 0 0 4 1 2021 0) "~V"))
(test-equal "~V gives ISO week 01 for 2019-12-30 (part of 2020-W01)"
            "01" (date->string (make-date 0 0 0 0 30 12 2019 0) "~V"))
(test-equal "~A names the weekday in full" "Saturday"
            (date->string (make-date 0 0 0 0 1 1 2000 0) "~A"))
(test-equal "~a abbreviates the weekday" "Sat"
            (date->string (make-date 0 0 0 0 1 1 2000 0) "~a"))
(test-equal "~B names the month in full" "January"
            (date->string (make-date 0 0 0 0 1 1 2000 0) "~B"))
(test-equal "~b abbreviates the month" "Jan"
            (date->string (make-date 0 0 0 0 1 1 2000 0) "~b"))
(test-equal "~j is the zero-padded day of year" "061"
            (date->string (make-date 0 0 0 0 1 3 2020 0) "~j"))
(test-equal "~e space-pads the day of month" " 1"
            (date->string (make-date 0 0 0 0 1 1 2000 0) "~e"))
(test-equal "~z prints the RFC-822 zone offset" "+0100"
            (date->string (make-date 0 0 0 0 1 1 2000 3600) "~z"))

;; ===========================================================================
;; Section K -- string->date parsing and the date->string round-trip
;; ===========================================================================
;; A round-trippable template must carry ~d ~m ~Y, since make-date needs
;; day/month/year; an incomplete template raises rather than fabricating them.

(test-equal "string->date reconstructs every field of an ISO instant"
            '(2006 5 4 3 2 1 0)
            (let ((d (string->date "2006-05-04T03:02:01Z" "~Y-~m-~dT~H:~M:~S~z")))
              (list (date-year d) (date-month d) (date-day d)
                    (date-hour d) (date-minute d) (date-second d)
                    (date-zone-offset d))))
(test-equal "date->string then string->date reproduces the field set"
            '(2021 3 14 15 9 26 0)
            (let* ((fmt "~Y-~m-~dT~H:~M:~S~z")
                   (d0 (make-date 0 26 9 15 14 3 2021 0))
                   (d (string->date (date->string d0 fmt) fmt)))
              (list (date-year d) (date-month d) (date-day d)
                    (date-hour d) (date-minute d) (date-second d)
                    (date-zone-offset d))))
(test-equal "string->date reads a named month via ~b" 3
            (date-month (string->date "14 Mar 2021" "~d ~b ~Y")))
(test-error "string->date rejects a template missing day/month/year"
            (string->date "03:02:01" "~H:~M:~S"))

(test-end)
