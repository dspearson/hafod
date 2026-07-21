#!chezscheme
;;; (hafod srfi-19) -- SRFI-19: Time Data Types and Procedures
;;; Reference: https://srfi.schemers.org/srfi-19/srfi-19.html
;;; Copyright (c) 2026 Dominic Pearson.
;;;
;;; Provides the SRFI 19 time and date data types with their comparison and
;;; arithmetic, UTC and TAI time with a leap-second table (refreshable from a
;;; USNO tai-utc.dat file via read-leap-second-table), Julian- and
;;; modified-Julian-day conversion in both directions, and date<->string over
;;; the standard output and input directive sets.

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
    date->string string->date current-date
    ;; Julian day
    date->julian-day date->modified-julian-day
    current-julian-day
    julian-day->date modified-julian-day->date
    ;; TAI / leap seconds
    time-utc->time-tai time-utc->time-tai!
    time-tai->time-utc time-tai->time-utc!
    read-leap-second-table)
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
      ;; date->julian-day already folds in the zone offset once, so epoch-seconds
      ;; is the absolute UTC instant -- do not subtract the offset a second time.
      (make-time time-utc (date-nanosecond d)
                 epoch-seconds)))

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
             ;; R6RS div already floors for a positive divisor, so era needs no
             ;; C++-style truncation bias -- adding one mis-decodes negative z
             ;; (pre-0000-03-01) dates by a day.
             (era (div z 146097))
             (doe (- z (* era 146097)))
             (yoe (div (- doe (div doe 1460) (- (div doe 36524)) (div doe 146096)) 365))
             (y (+ yoe (* era 400)))
             (doy (- doe (+ (* 365 yoe) (div yoe 4) (- (div yoe 100)))))
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

  ;; ---------------------------------------------------------------------------
  ;; date->string and the full SRFI 19 output-directive set
  ;; ---------------------------------------------------------------------------
  ;; The SRFI 19 default (English) locale name vectors.  The month vectors are
  ;; 1-based (index 0 is unused) to index directly by date-month.
  (define *abbr-weekday*
    '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
  (define *long-weekday*
    '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
  (define *abbr-month*
    '#("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
  (define *long-month*
    '#("" "January" "February" "March" "April" "May" "June"
       "July" "August" "September" "October" "November" "December"))

  ;; Left-pad the decimal string of n to `width` columns with `pad-with`.  A
  ;; pad-with of #f (from the ~- modifier) suppresses padding; a string already
  ;; at or over width is returned unchanged.
  (define (padding n pad-with width)
    (let* ((str (number->string n))
           (len (string-length str)))
      (if (or (> len width) (not pad-with))
          str
          (string-append (make-string (- width len) pad-with) str))))

  (define (last-n-digits i n)
    (mod (abs i) (expt 10 n)))

  (define (strip-trailing-zeros s)
    (let loop ((k (string-length s)))
      (if (and (> k 0) (char=? (string-ref s (- k 1)) #\0))
          (loop (- k 1))
          (substring s 0 k))))

  ;; 12-hour clock: midnight and noon both print 12.
  (define (hour12 hr)
    (cond ((zero? hr) 12)
          ((> hr 12) (- hr 12))
          (else hr)))

  ;; RFC-822 zone: offset 0 prints "Z", otherwise a signed HHMM.
  (define (tz-printer offset port)
    (cond ((= offset 0) (write-char #\Z port))
          ((negative? offset) (write-char #\- port))
          (else (write-char #\+ port)))
    (unless (= offset 0)
      (display (padding (abs (quotient offset 3600)) #\0 2) port)
      (display (padding (abs (quotient (remainder offset 3600) 60)) #\0 2) port)))

  ;; Week-number helpers (~U/~W/~V).  date-week-day is 0=Sunday..6=Saturday.
  (define (days-before-first-week d start-dow)
    (let ((first-dow (date-week-day (make-date 0 0 0 0 1 1 (date-year d) 0))))
      (mod (- start-dow first-dow) 7)))

  (define (date-week-number d start-dow)
    (div (- (date-year-day d) (days-before-first-week d start-dow)) 7))

  (define (date-week-number-iso d)
    (let* ((first-dow (date-week-day (make-date 0 0 0 0 1 1 (date-year d) 0)))
           (offset (if (> first-dow 4) 0 1))
           (w (+ (div (+ (date-year-day d) first-dow -2) 7) offset)))
      (cond ((zero? w)
             (date-week-number-iso
               (make-date 0 0 0 0 31 12 (- (date-year d) 1) 0)))
            ((and (= w 53)
                  (<= (date-week-day
                        (make-date 0 0 0 0 1 1 (+ (date-year d) 1) 0)) 4))
             1)
            (else w))))

  ;; ~f: the second (with the > 1e9 carry) then, if non-zero, the locale
  ;; decimal point and the fractional digits with trailing zeros trimmed.
  (define (emit-seconds-fraction d pad-with port)
    (let ((ns (date-nanosecond d)))
      (display (padding (+ (date-second d) (if (> ns 1000000000) 1 0))
                        pad-with 2) port)
      (let ((frac (strip-trailing-zeros (padding (mod ns 1000000000) #\0 9))))
        (when (> (string-length frac) 0)
          (write-char #\. port)
          (display frac port)))))

  ;; Emit a single directive.  pad-with is #\0 (default), #\space (~_) or #f
  ;; (~-).  Composite directives recurse through format-date on a sub-format.
  (define (emit-directive d ch pad-with port)
    (case ch
      ((#\~) (write-char #\~ port))
      ((#\a) (display (vector-ref *abbr-weekday* (date-week-day d)) port))
      ((#\A) (display (vector-ref *long-weekday* (date-week-day d)) port))
      ((#\b #\h) (display (vector-ref *abbr-month* (date-month d)) port))
      ((#\B) (display (vector-ref *long-month* (date-month d)) port))
      ((#\c) (format-date d "~a ~b ~d ~H:~M:~S~z ~Y" port))
      ((#\d) (display (padding (date-day d) pad-with 2) port))
      ((#\D) (format-date d "~m/~d/~y" port))
      ((#\e) (display (padding (date-day d) #\space 2) port))
      ((#\f) (emit-seconds-fraction d pad-with port))
      ((#\H) (display (padding (date-hour d) pad-with 2) port))
      ((#\I) (display (padding (hour12 (date-hour d)) pad-with 2) port))
      ((#\j) (display (padding (date-year-day d) pad-with 3) port))
      ((#\k) (display (padding (date-hour d) #\space 2) port))
      ((#\l) (display (padding (hour12 (date-hour d)) #\space 2) port))
      ((#\m) (display (padding (date-month d) pad-with 2) port))
      ((#\M) (display (padding (date-minute d) pad-with 2) port))
      ((#\n) (write-char #\newline port))
      ((#\N) (display (padding (date-nanosecond d) pad-with 9) port))
      ((#\p) (display (if (> (date-hour d) 11) "PM" "AM") port))
      ((#\r) (format-date d "~I:~M:~S ~p" port))
      ((#\s) (display (number->string (time-second (date->time-utc d))) port))
      ((#\S) (display (padding (+ (date-second d)
                                  (if (> (date-nanosecond d) 1000000000) 1 0))
                               pad-with 2) port))
      ((#\t) (write-char #\tab port))
      ((#\T) (format-date d "~H:~M:~S" port))
      ((#\U) (let ((dbfw (days-before-first-week d 0)))
               (display (padding (+ (date-week-number d 0) (if (> dbfw 0) 1 0))
                                 pad-with 2) port)))
      ((#\V) (display (padding (date-week-number-iso d) pad-with 2) port))
      ((#\w) (display (number->string (date-week-day d)) port))
      ((#\W) (let ((dbfw (days-before-first-week d 1)))
               (display (padding (+ (date-week-number d 1) (if (> dbfw 0) 1 0))
                                 pad-with 2) port)))
      ((#\x) (format-date d "~m/~d/~y" port))
      ((#\X) (format-date d "~H:~M:~S" port))
      ((#\y) (display (padding (last-n-digits (date-year d) 2) pad-with 2) port))
      ((#\Y) (display (padding (date-year d) pad-with 4) port))
      ((#\z) (tz-printer (date-zone-offset d) port))
      ((#\Z) (if #f #f))        ; symbol time zone -- a no-op, per the reference
      ((#\1) (format-date d "~Y-~m-~d" port))
      ((#\2) (format-date d "~H:~M:~S~z" port))
      ((#\3) (format-date d "~H:~M:~S" port))
      ((#\4) (format-date d "~Y-~m-~dT~H:~M:~S~z" port))
      ((#\5) (format-date d "~Y-~m-~dT~H:~M:~S" port))
      ;; An unrecognised directive is passed through verbatim (~ + char).
      (else (write-char #\~ port) (write-char ch port))))

  ;; Walk a format string, honouring the ~- (no pad) and ~_ (space pad)
  ;; modifiers before each directive; the default pad is #\0.
  (define (format-date d fmt port)
    (let ((len (string-length fmt)))
      (let loop ((i 0))
        (when (< i len)
          (let ((ch (string-ref fmt i)))
            (cond
              ((not (char=? ch #\~))
               (write-char ch port)
               (loop (+ i 1)))
              ((>= (+ i 1) len)
               ;; a trailing lone tilde is emitted literally
               (write-char #\~ port)
               (loop (+ i 1)))
              (else
               (let ((next (string-ref fmt (+ i 1))))
                 (cond
                   ((and (char=? next #\-) (< (+ i 2) len))
                    (emit-directive d (string-ref fmt (+ i 2)) #f port)
                    (loop (+ i 3)))
                   ((and (char=? next #\_) (< (+ i 2) len))
                    (emit-directive d (string-ref fmt (+ i 2)) #\space port)
                    (loop (+ i 3)))
                   (else
                    (emit-directive d next #\0 port)
                    (loop (+ i 2))))))))))))

  (define date->string
    (case-lambda
      ((d) (date->string d "~Y-~m-~d ~H:~M:~S"))
      ((d fmt)
       (let ((out (open-output-string)))
         (format-date d fmt out)
         (get-output-string out)))))

  ;; ---------------------------------------------------------------------------
  ;; Leap seconds and UTC <-> TAI conversion
  ;; ---------------------------------------------------------------------------
  ;; The standard historical UTC-TAI difference, most-recent row first, ending at
  ;; the 1972-01-01 baseline.  Each entry pairs a UTC instant (Unix seconds since
  ;; 1970-01-01) with the whole-second offset in force from that instant onwards.
  ;; No leap second has been announced since 2017; read-leap-second-table
  ;; refreshes the table from a USNO tai-utc.dat file.  The variable stays
  ;; module-local (never exported) so a refresh can swap it wholesale.
  (define *leap-second-table*
    '((1483228800 . 37)   ; 2017-01-01
      (1435708800 . 36)   ; 2015-07-01
      (1341100800 . 35)   ; 2012-07-01
      (1230768000 . 34)   ; 2009-01-01
      (1136073600 . 33)   ; 2006-01-01
      (915148800  . 32)   ; 1999-01-01
      (867715200  . 31)   ; 1997-07-01
      (820454400  . 30)   ; 1996-01-01
      (773020800  . 29)   ; 1994-07-01
      (741484800  . 28)   ; 1993-07-01
      (709948800  . 27)   ; 1992-07-01
      (662688000  . 26)   ; 1991-01-01
      (631152000  . 25)   ; 1990-01-01
      (567993600  . 24)   ; 1988-01-01
      (489024000  . 23)   ; 1985-07-01
      (425865600  . 22)   ; 1983-07-01
      (394329600  . 21)   ; 1982-07-01
      (362793600  . 20)   ; 1981-07-01
      (315532800  . 19)   ; 1980-01-01
      (283996800  . 18)   ; 1979-01-01
      (252460800  . 17)   ; 1978-01-01
      (220924800  . 16)   ; 1977-01-01
      (189302400  . 15)   ; 1976-01-01
      (157766400  . 14)   ; 1975-01-01
      (126230400  . 13)   ; 1974-01-01
      (94694400   . 12)   ; 1973-01-01
      (78796800   . 11)   ; 1972-07-01
      (63072000   . 10))) ; 1972-01-01  (baseline row)

  ;; UTC-second boundary below which the offset is nil, per the spec's clamp
  ;; (= (* 2 365 86400) = the naive count of seconds from 1970 to 1972).
  (define *leap-second-base* (* (- 1972 1970) 365 86400))

  ;; UTC -> TAI: the accumulated offset in force at utc-seconds.
  (define (leap-second-delta utc-seconds)
    (if (< utc-seconds *leap-second-base*)
        0
        (let lsd ((table *leap-second-table*))
          (cond ((>= utc-seconds (caar table)) (cdar table))
                (else (lsd (cdr table)))))))

  ;; TAI -> UTC: the offset to subtract to recover the UTC instant.
  (define (leap-second-neg-delta tai-seconds)
    (if (< tai-seconds *leap-second-base*)
        0
        (let lsd ((table *leap-second-table*))
          (cond ((null? table) 0)
                ((<= (cdar table) (- tai-seconds (caar table))) (cdar table))
                (else (lsd (cdr table)))))))

  (define (time-utc->time-tai t)
    (unless (eq? (time-type t) time-utc)
      (assertion-violation 'time-utc->time-tai "not a time-utc" t))
    (make-time time-tai (time-nanosecond t)
               (+ (time-second t) (leap-second-delta (time-second t)))))

  (define (time-tai->time-utc t)
    (unless (eq? (time-type t) time-tai)
      (assertion-violation 'time-tai->time-utc "not a time-tai" t))
    (make-time time-utc (time-nanosecond t)
               (- (time-second t) (leap-second-neg-delta (time-second t)))))

  ;; hafod's time records are immutable, so the bang variants return a fresh
  ;; record just like the non-bang forms -- the same convention as add-duration!
  ;; and time-difference!.
  (define time-utc->time-tai! time-utc->time-tai)
  (define time-tai->time-utc! time-tai->time-utc)

  ;; Parse a USNO tai-utc.dat stream from a port.  A line reads as e.g.
  ;;   1972 JAN 1 =JD 2441317.5 TAI-UTC= 10.0 S + (MJD - 41317.) X 0.0 S
  ;; so the year is the first token, the Julian day the token after "=JD", and
  ;; the offset the token after "TAI-UTC=".  read consumes DATA tokens only and
  ;; never evaluates; each line is guarded so a malformed row is dropped rather
  ;; than aborting the parse.  Rows are consed newest-at-head, matching the
  ;; embedded table's most-recent-first ordering.
  (define (read-tai-utc-data port)
    (let loop ((line (get-line port)) (table '()))
      (if (eof-object? line)
          table
          (let ((row (guard (exn (#t #f))
                       (let* ((data (read (open-input-string
                                            (string-append "(" line ")"))))
                              (year (car data))
                              (jd   (list-ref data 4))    ; token after =JD
                              (secs (list-ref data 6)))   ; token after TAI-UTC=
                         (and (number? year) (number? jd) (number? secs)
                              (>= year 1972)
                              (cons (* (- (inexact->exact jd) 4881175/2) 86400)
                                    (inexact->exact secs)))))))
            (loop (get-line port) (if row (cons row table) table))))))

  ;; Refresh *leap-second-table* from a tai-utc.dat file.  The file open is
  ;; guarded and the whole operation fails closed: on a missing file or an empty
  ;; parse the embedded table is left intact.
  (define (read-leap-second-table filename)
    (let ((rows (guard (exn (#t #f))
                  (let ((port (open-input-file filename)))
                    (let ((data (read-tai-utc-data port)))
                      (close-input-port port)
                      data)))))
      (when (and (list? rows) (pair? rows))
        (set! *leap-second-table* rows))))

  ;; ---------------------------------------------------------------------------
  ;; Julian-day and modified-Julian-day inverse conversions
  ;; ---------------------------------------------------------------------------
  ;; Inverses of date->julian-day / date->modified-julian-day.  4881175/2 is the
  ;; Julian day of the Unix epoch and 4800001/2 the JD-MJD difference -- exactly
  ;; the constants the forward conversions use, so the round trip is closed.  The
  ;; civil decode is delegated to time-utc->date, which already round-trips.
  (define (julian-day->time-utc jd)
    (let ((total-ns (exact (round (* 1000000000 86400 (- jd 4881175/2))))))
      (make-time time-utc
                 (mod total-ns 1000000000)
                 (div total-ns 1000000000))))

  (define (julian-day->date jd . tz)
    (time-utc->date (julian-day->time-utc jd) (if (pair? tz) (car tz) 0)))

  (define (modified-julian-day->date mjd . tz)
    (apply julian-day->date (+ mjd 4800001/2) tz))

  ;; ---------------------------------------------------------------------------
  ;; string->date -- parse a formatted date string over the input directive set
  ;; ---------------------------------------------------------------------------
  ;; hafod's date records are immutable, so the parser accumulates fields into a
  ;; scratch vector (in make-date's argument order) and builds the date once at
  ;; the end -- rather than mutating a partially-built record as the reference
  ;; implementation does.

  (define (char->int ch)
    (- (char->integer ch) (char->integer #\0)))

  (define (next-digit port)
    (let ((c (read-char port)))
      (if (eof-object? c)
          (assertion-violation 'string->date "unexpected end of input" #f)
          (char->int c))))

  ;; Read the run of alphabetic characters at the port (a weekday or month name).
  (define (read-alpha-token port)
    (let ((out (open-output-string)))
      (let loop ()
        (let ((ch (peek-char port)))
          (if (and (not (eof-object? ch)) (char-alphabetic? ch))
              (begin (write-char (read-char port) out) (loop))
              (get-output-string out))))))

  (define (vector-name-index vec name)
    (let loop ((i 1))
      (cond ((>= i (vector-length vec)) #f)
            ((string=? (vector-ref vec i) name) i)
            (else (loop (+ i 1))))))

  ;; A month name -> its 1..12 index, looked up in both the abbreviated and full
  ;; locale vectors so either spelling parses.
  (define (month-name->index name)
    (or (vector-name-index *abbr-month* name)
        (vector-name-index *long-month* name)
        (assertion-violation 'string->date "unknown month name" name)))

  ;; Expand a two-digit year within a 50-year window of the current year (~y).
  (define (natural-year n)
    (let* ((current-year (date-year (current-date)))
           (century (* (div current-year 100) 100)))
      (cond ((>= n 100) n)
            ((< n 0) n)
            ((<= (- (+ century n) current-year) 50) (+ century n))
            (else (+ (- century 100) n)))))

  ;; An integer reader consuming at most `upto` digits (#f for unbounded).
  (define (make-integer-reader upto)
    (lambda (port)
      (let loop ((accum 0) (nchars 0))
        (let ((ch (peek-char port)))
          (if (or (eof-object? ch)
                  (not (char-numeric? ch))
                  (and upto (>= nchars upto)))
              accum
              (begin (read-char port)
                     (loop (+ (* accum 10) (char->int ch)) (+ nchars 1))))))))

  ;; An exact-columns reader consuming `n` columns, tolerating leading blanks
  ;; (~e / ~k / ~y are space-padded fields).
  (define (make-integer-exact-reader n)
    (lambda (port)
      (let loop ((accum 0) (nchars 0) (pad-ok #t))
        (if (>= nchars n)
            accum
            (let ((ch (peek-char port)))
              (cond
                ((eof-object? ch) accum)
                ((char-numeric? ch)
                 (read-char port)
                 (loop (+ (* accum 10) (char->int ch)) (+ nchars 1) #f))
                ((and pad-ok (char=? ch #\space))
                 (read-char port)
                 (loop accum (+ nchars 1) #t))
                (else accum)))))))

  ;; A fractional reader (~N): up to `upto` digits, normalised to `upto` columns
  ;; so "123" over 9 columns yields 123000000 nanoseconds.
  (define (make-fractional-reader upto)
    (lambda (port)
      (let loop ((accum 0) (nchars 0))
        (let ((ch (peek-char port)))
          (if (or (eof-object? ch)
                  (not (char-numeric? ch))
                  (>= nchars upto))
              (* accum (expt 10 (- upto nchars)))
              (begin (read-char port)
                     (loop (+ (* accum 10) (char->int ch)) (+ nchars 1))))))))

  ;; A reader that consumes one specific literal character (~~).
  (define (make-char-id-reader wanted)
    (lambda (port)
      (let ((c (read-char port)))
        (if (and (char? c) (char=? c wanted))
            wanted
            (assertion-violation 'string->date "expected literal tilde" wanted)))))

  ;; Zone reader (~z): "Z"/"z" is UTC; otherwise a sign then HH[:]MM.
  (define (zone-reader port)
    (let ((ch (read-char port)))
      (cond
        ((eof-object? ch)
         (assertion-violation 'string->date "unexpected end of input in zone" #f))
        ((or (char=? ch #\Z) (char=? ch #\z)) 0)
        ((or (char=? ch #\+) (char=? ch #\-))
         (let* ((sign (if (char=? ch #\+) 1 -1))
                (h10 (next-digit port))
                (h1  (next-digit port))
                (nxt (read-char port)))
           (when (eof-object? nxt)
             (assertion-violation 'string->date
                                  "unexpected end of input in zone" #f))
           (let* ((m10 (if (char=? nxt #\:) (next-digit port) (char->int nxt)))
                  (m1  (next-digit port)))
             (* sign (+ (* (+ (* h10 10) h1) 3600)
                        (* (+ (* m10 10) m1) 60))))))
        (else
         (assertion-violation 'string->date "bad zone designator" ch)))))

  ;; Skip input characters until `skipper` accepts one (or EOF is reached).
  (define (skip-until port skipper)
    (let ((ch (peek-char port)))
      (cond ((eof-object? ch) #t)
            ((skipper ch) #t)
            (else (read-char port) (skip-until port skipper)))))

  ;; The input-directive table: (char skipper reader actor).  The actor stores
  ;; the read value into the scratch vector; ~a/~A read and discard the weekday.
  ;; Scratch slots: 0=nanosecond 1=second 2=minute 3=hour 4=day 5=month 6=year
  ;; 7=zone -- exactly make-date's argument order.
  (define *string->date-directives*
    (let ((skip-nothing  (lambda (ch) #t))
          (zone-start     (lambda (c) (or (char=? c #\Z) (char=? c #\z)
                                          (char=? c #\+) (char=? c #\-))))
          (int2           (make-integer-reader 2))
          (int4           (make-integer-reader 4))
          (exact2         (make-integer-exact-reader 2))
          (frac9          (make-fractional-reader 9))
          (weekday-reader (lambda (port) (read-alpha-token port)))
          (month-reader   (lambda (port) (month-name->index (read-alpha-token port))))
          (do-nothing     (lambda (v acc) (if #f #f)))
          (set-day        (lambda (v acc) (vector-set! acc 4 v)))
          (set-month      (lambda (v acc) (vector-set! acc 5 v)))
          (set-hour       (lambda (v acc) (vector-set! acc 3 v)))
          (set-minute     (lambda (v acc) (vector-set! acc 2 v)))
          (set-second     (lambda (v acc) (vector-set! acc 1 v)))
          (set-nanosecond (lambda (v acc) (vector-set! acc 0 v)))
          (set-year       (lambda (v acc) (vector-set! acc 6 v)))
          (set-nat-year   (lambda (v acc) (vector-set! acc 6 (natural-year v))))
          (set-zone       (lambda (v acc) (vector-set! acc 7 v))))
      (list
        (list #\~ skip-nothing (make-char-id-reader #\~) do-nothing)
        (list #\a char-alphabetic? weekday-reader do-nothing)
        (list #\A char-alphabetic? weekday-reader do-nothing)
        (list #\b char-alphabetic? month-reader set-month)
        (list #\B char-alphabetic? month-reader set-month)
        (list #\h char-alphabetic? month-reader set-month)
        (list #\d char-numeric? int2 set-day)
        (list #\e skip-nothing exact2 set-day)
        (list #\H char-numeric? int2 set-hour)
        (list #\k skip-nothing exact2 set-hour)
        (list #\m char-numeric? int2 set-month)
        (list #\M char-numeric? int2 set-minute)
        (list #\N char-numeric? frac9 set-nanosecond)
        (list #\S char-numeric? int2 set-second)
        (list #\y skip-nothing exact2 set-nat-year)
        (list #\Y char-numeric? int4 set-year)
        (list #\z zone-start zone-reader set-zone))))

  (define (string->date input template)
    (let ((acc (vector 0 0 0 0 #f #f #f 0))    ; ns s mi h d m y z; zone defaults 0/UTC
          (port (open-input-string input))
          (tlen (string-length template)))
      (let loop ((i 0))
        (when (< i tlen)
          (let ((ch (string-ref template i)))
            (cond
              ((not (char=? ch #\~))
               ;; a literal template character must match the input
               (let ((pc (read-char port)))
                 (when (or (eof-object? pc) (not (char=? pc ch)))
                   (assertion-violation 'string->date
                                        "input does not match template literal" ch)))
               (loop (+ i 1)))
              ((>= (+ i 1) tlen)
               (assertion-violation 'string->date "dangling ~ in template" template))
              (else
               (let* ((dch (string-ref template (+ i 1)))
                      (entry (assv dch *string->date-directives*)))
                 (unless entry
                   (assertion-violation 'string->date "unknown input directive" dch))
                 (skip-until port (cadr entry))
                 ((cadddr entry) ((caddr entry) port) acc)
                 (loop (+ i 2))))))))
      ;; date-ok?: make-date needs day, month and year filled
      (unless (and (vector-ref acc 4) (vector-ref acc 5) (vector-ref acc 6))
        (assertion-violation 'string->date
                             "incomplete date: need day, month and year" template))
      (apply make-date (vector->list acc))))
  )
