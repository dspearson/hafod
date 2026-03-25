#!chezscheme
;;; (hafod srfi-19) -- SRFI-19: Time Data Types and Procedures
;;; Reference: https://srfi.schemers.org/srfi-19/srfi-19.html
;;; Copyright (c) 2026 Dominic Pearson.

(library (hafod srfi-19)
  (export
    ;; Time types
    time-duration time-monotonic time-process time-tai time-thread time-utc
    ;; Time constructors/accessors
    make-time time? time-type time-second time-nanosecond
    copy-time
    ;; Current time
    current-time
    ;; Time comparison
    time=? time<? time>? time<=? time>=?
    ;; Time arithmetic
    time-difference time-difference! add-duration add-duration!
    subtract-duration subtract-duration!
    ;; Date
    make-date date? date-nanosecond date-second date-minute date-hour
    date-day date-month date-year date-zone-offset
    date-year-day date-week-day
    ;; Conversions
    date->time-utc time-utc->date
    date->string current-date
    ;; Julian day
    date->julian-day date->modified-julian-day
    current-julian-day)
  (import (except (chezscheme)
                  make-time time? time-type time-second time-nanosecond
                  copy-time current-time
                  time=? time<? time>? time<=? time>=?
                  time-difference time-difference!
                  add-duration add-duration!
                  subtract-duration subtract-duration!
                  make-date date? date-nanosecond date-second date-minute
                  date-hour date-day date-month date-year date-zone-offset
                  date-year-day date-week-day
                  date->time-utc time-utc->date
                  current-date))

  ;; Alias Chez current-time before we shadow it
  (module chez-time (chez:current-time chez:time-second chez:time-nanosecond)
    (import (only (chezscheme) current-time time-second time-nanosecond))
    (define chez:current-time current-time)
    (define chez:time-second time-second)
    (define chez:time-nanosecond time-nanosecond))
  (import chez-time)

  ;; Time type tags
  (define time-duration 'time-duration)
  (define time-monotonic 'time-monotonic)
  (define time-process 'time-process)
  (define time-tai 'time-tai)
  (define time-thread 'time-thread)
  (define time-utc 'time-utc)

  ;; Time record
  (define-record-type srfi19-time
    (fields (immutable type) (immutable second) (immutable nanosecond))
    (nongenerative srfi19-time-hafod))

  (define (make-time type nanosecond second)
    (make-srfi19-time type second nanosecond))

  (define time? srfi19-time?)
  (define time-type srfi19-time-type)
  (define time-second srfi19-time-second)
  (define time-nanosecond srfi19-time-nanosecond)

  (define (copy-time t)
    (make-time (time-type t) (time-nanosecond t) (time-second t)))

  ;; Current time using Chez's built-in current-time
  (define current-time
    (case-lambda
      (() (current-time time-utc))
      ((type)
       (let ((t (chez:current-time)))
         (make-time type
                    (chez:time-nanosecond t)
                    (chez:time-second t))))))

  ;; Time comparison
  (define (time=? a b)
    (and (= (time-second a) (time-second b))
         (= (time-nanosecond a) (time-nanosecond b))))

  (define (time<? a b)
    (or (< (time-second a) (time-second b))
        (and (= (time-second a) (time-second b))
             (< (time-nanosecond a) (time-nanosecond b)))))

  (define (time>? a b) (time<? b a))
  (define (time<=? a b) (not (time>? a b)))
  (define (time>=? a b) (not (time<? a b)))

  ;; Time arithmetic
  (define (time-difference a b)
    (let* ((sns (- (time-second a) (time-second b)))
           (nns (- (time-nanosecond a) (time-nanosecond b))))
      (if (< nns 0)
          (make-time time-duration (+ nns 1000000000) (- sns 1))
          (make-time time-duration nns sns))))

  (define time-difference! time-difference)

  (define (add-duration t dur)
    (let* ((sns (+ (time-second t) (time-second dur)))
           (nns (+ (time-nanosecond t) (time-nanosecond dur))))
      (if (>= nns 1000000000)
          (make-time (time-type t) (- nns 1000000000) (+ sns 1))
          (make-time (time-type t) nns sns))))

  (define add-duration! add-duration)

  (define (subtract-duration t dur)
    (let* ((sns (- (time-second t) (time-second dur)))
           (nns (- (time-nanosecond t) (time-nanosecond dur))))
      (if (< nns 0)
          (make-time (time-type t) (+ nns 1000000000) (- sns 1))
          (make-time (time-type t) nns sns))))

  (define subtract-duration! subtract-duration)

  ;; Date record
  (define-record-type srfi19-date
    (fields nanosecond second minute hour day month year zone-offset)
    (nongenerative srfi19-date-hafod))

  (define (make-date nanosecond second minute hour day month year zone-offset)
    (make-srfi19-date nanosecond second minute hour day month year zone-offset))

  (define date? srfi19-date?)
  (define date-nanosecond srfi19-date-nanosecond)
  (define date-second srfi19-date-second)
  (define date-minute srfi19-date-minute)
  (define date-hour srfi19-date-hour)
  (define date-day srfi19-date-day)
  (define date-month srfi19-date-month)
  (define date-year srfi19-date-year)
  (define date-zone-offset srfi19-date-zone-offset)

  ;; Days in each month (non-leap)
  (define *month-days* '#(0 31 28 31 30 31 30 31 31 30 31 30 31))

  (define (leap-year? y)
    (and (zero? (mod y 4))
         (or (not (zero? (mod y 100)))
             (zero? (mod y 400)))))

  (define (days-in-month m y)
    (if (and (= m 2) (leap-year? y)) 29
        (vector-ref *month-days* m)))

  (define (date-year-day d)
    (let ((m (date-month d)) (y (date-year d)))
      (let loop ((i 1) (total 0))
        (if (= i m) (+ total (date-day d))
            (loop (+ i 1) (+ total (days-in-month i y)))))))

  (define (date-week-day d)
    ;; Tomohiko Sakamoto's algorithm
    (let* ((y (date-year d))
           (m (date-month d))
           (day (date-day d))
           (t '#(0 3 2 5 0 3 5 1 4 6 2 4))
           (y (if (< m 3) (- y 1) y)))
      (mod (+ y (div y 4) (- (div y 100)) (div y 400)
              (vector-ref t (- m 1)) day)
           7)))

  ;; date->time-utc
  (define (date->time-utc d)
    (let* ((jd (date->julian-day d))
           (epoch-seconds (exact (round (* (- jd 4881175/2) 86400)))))
      (make-time time-utc (date-nanosecond d)
                 (- epoch-seconds (date-zone-offset d)))))

  ;; time-utc->date
  (define (time-utc->date t . tz-offset)
    (let* ((offset (if (pair? tz-offset) (car tz-offset) 0))
           (secs (+ (time-second t) offset))
           (ns (time-nanosecond t)))
      (epoch->date secs ns offset)))

  (define (epoch->date secs ns offset)
    (let* ((days (div secs 86400))
           (rem (mod secs 86400))
           (rem (if (< rem 0) (begin (set! days (- days 1)) (+ rem 86400)) rem))
           (hour (div rem 3600))
           (rem2 (mod rem 3600))
           (minute (div rem2 60))
           (second (mod rem2 60)))
      ;; Howard Hinnant's civil_from_days
      (let* ((z (+ days 719468))
             (era (div (if (>= z 0) z (- z 146096)) 146097))
             (doe (- z (* era 146097)))
             (yoe (div (- doe (div doe 1460) (- (div doe 36524)) (div doe 146096)) 365))
             (y (+ yoe (* era 400)))
             (doy (- doe (- (* 365 yoe) (div yoe 4) (- (div yoe 100)))))
             (mp (div (+ (* 5 doy) 2) 153))
             (day (+ (- doy (div (+ (* 153 mp) 2) 5)) 1))
             (month (+ mp (if (< mp 10) 3 -9)))
             (year (+ y (if (<= month 2) 1 0))))
        (make-date ns second minute hour day month year offset))))

  ;; current-date
  (define current-date
    (case-lambda
      (() (current-date 0))
      ((tz-offset) (time-utc->date (current-time time-utc) tz-offset))))

  ;; Julian day
  (define (date->julian-day d)
    (let* ((y (date-year d))
           (m (date-month d))
           (day (date-day d))
           (a (div (- 14 m) 12))
           (y1 (+ y 4800 (- a)))
           (m1 (+ m (* 12 a) -3)))
      (+ day
         (div (+ (* 153 m1) 2) 5)
         (* 365 y1)
         (div y1 4)
         (- (div y1 100))
         (div y1 400)
         -32045 -1/2
         (/ (+ (* (date-hour d) 3600)
               (* (date-minute d) 60)
               (date-second d)
               (- (date-zone-offset d)))
            86400))))

  (define (date->modified-julian-day d)
    (- (date->julian-day d) 4800001/2))

  (define (current-julian-day)
    (date->julian-day (current-date)))

  ;; date->string
  (define date->string
    (case-lambda
      ((d) (date->string d "~Y-~m-~d ~H:~M:~S"))
      ((d fmt)
       (let ((out (open-output-string)))
         (let loop ((i 0))
           (when (< i (string-length fmt))
             (if (and (char=? (string-ref fmt i) #\~)
                      (< (+ i 1) (string-length fmt)))
                 (let ((c (string-ref fmt (+ i 1))))
                   (case c
                     ((#\Y) (display (pad4 (date-year d)) out))
                     ((#\m) (display (pad2 (date-month d)) out))
                     ((#\d) (display (pad2 (date-day d)) out))
                     ((#\H) (display (pad2 (date-hour d)) out))
                     ((#\M) (display (pad2 (date-minute d)) out))
                     ((#\S) (display (pad2 (date-second d)) out))
                     ((#\~) (display "~" out))
                     (else (display "~" out) (display c out)))
                   (loop (+ i 2)))
                 (begin
                   (display (string-ref fmt i) out)
                   (loop (+ i 1))))))
         (get-output-string out)))))

  (define (pad2 n)
    (let ((s (number->string n)))
      (if (< (string-length s) 2)
          (string-append "0" s) s)))

  (define (pad4 n)
    (let ((s (number->string n)))
      (cond
        ((< (string-length s) 4)
         (string-append (make-string (- 4 (string-length s)) #\0) s))
        (else s)))))
