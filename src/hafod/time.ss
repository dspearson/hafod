;;; (hafod time) -- Date/time operations
;;; Provides date record type, time/date conversion, and date formatting.
;;; Ported from scsh/scheme/time.scm

(library (hafod time)
  (export
    ;; Date record
    make-date date?
    date:seconds date:minute date:hour date:month-day date:month date:year
    date:tz-name date:tz-secs date:summer? date:week-day date:year-day
    ;; Date setters (scsh-compatible)
    set-date:seconds set-date:minute set-date:hour set-date:month-day
    set-date:month set-date:year set-date:tz-name set-date:tz-secs
    set-date:summer? set-date:week-day set-date:year-day

    ;; Time/date functions
    time date date->string format-date

    ;; Sub-second time resolution
    time+ticks ticks/sec)

  (import (except (hafod internal base)
                   date-second date-minute date-hour date-day
                   date-month date-year date-week-day date-year-day
                   date-zone-offset date-zone-name date-dst?
                   date-and-time current-date)
          (hafod posix) (hafod compat))

  ;; ======================================================================
  ;; Date record type
  ;; ======================================================================

  ;; scsh date has 11 fields:
  ;; seconds, minute, hour, month-day, month, year,
  ;; tz-name, tz-secs, summer?, week-day, year-day
  ;;
  ;; Use %date as the internal record type name to avoid clashing with the
  ;; (define (date ...) ...) function. Export make-date, date?, and accessors
  ;; using the %date-based generated names, aliased to scsh names.
  (define-record-type (%date %make-date date?)
    (fields (mutable seconds)
            (mutable minute)
            (mutable hour)
            (mutable month-day)
            (mutable month)
            (mutable year)
            (mutable tz-name)
            (mutable tz-secs)
            (mutable summer?)
            (mutable week-day)
            (mutable year-day)))

  ;; make-date: variable-arity constructor matching scsh conventions.
  ;; At least 6 args (sec min hour mday month year); up to 11 args.
  (define (make-date s mi h md mo y . rest)
    (let-optionals* rest ([tz-name #f] [tz-secs #f] [summer? #f] [wd 0] [yd 0])
      (%make-date s mi h md mo y tz-name tz-secs summer? wd yd)))

  ;; scsh-compatible accessor names
  (define date:seconds %date-seconds)
  (define date:minute %date-minute)
  (define date:hour %date-hour)
  (define date:month-day %date-month-day)
  (define date:month %date-month)
  (define date:year %date-year)
  (define date:tz-name %date-tz-name)
  (define date:tz-secs %date-tz-secs)
  (define date:summer? %date-summer?)
  (define date:week-day %date-week-day)
  (define date:year-day %date-year-day)

  ;; scsh-compatible setter names
  (define set-date:seconds %date-seconds-set!)
  (define set-date:minute %date-minute-set!)
  (define set-date:hour %date-hour-set!)
  (define set-date:month-day %date-month-day-set!)
  (define set-date:month %date-month-set!)
  (define set-date:year %date-year-set!)
  (define set-date:tz-name %date-tz-name-set!)
  (define set-date:tz-secs %date-tz-secs-set!)
  (define set-date:summer? %date-summer?-set!)
  (define set-date:week-day %date-week-day-set!)
  (define set-date:year-day %date-year-day-set!)

  ;; ======================================================================
  ;; Time
  ;; ======================================================================

  ;; (time) => current epoch seconds
  ;; (time date-obj) => epoch seconds from date
  (define (time . args)
    (if (null? args)
        (posix-time)
        (let ([d (check-arg date? (car args) time)])
          ;; Convert date to epoch via mktime
          ;; mktime expects: sec min hour mday mon(0-11) year(since 1900) isdst
          (posix-mktime
            (date:seconds d)
            (date:minute d)
            (date:hour d)
            (date:month-day d)
            (date:month d)
            (- (date:year d) 1900)
            (if (date:summer? d) 1 (if (eq? (date:summer? d) #f) -1 0))))))

  ;; ======================================================================
  ;; Date
  ;; ======================================================================

  ;; (date) => current local date
  ;; (date epoch-secs) => local date for that time
  ;; (date epoch-secs zone) => date in specified timezone
  ;; zone can be: #f (local), integer (seconds offset from UTC), or string (tz name)
  (define (date . args)
    (let ([epoch (if (pair? args)
                     (car args)
                     (posix-time))])
      ;; For v1, only support local time and UTC.
      ;; zone = #f or absent => local, zone = 0 or "UTC" => UTC
      (let ([zone (if (and (pair? args) (pair? (cdr args)))
                      (cadr args)
                      #f)])
        (receive (sec min hour mday mon year wday yday isdst gmtoff tz-name)
          (if (and zone (or (and (integer? zone) (zero? zone))
                            (and (string? zone) (string=? zone "UTC"))))
              (posix-gmtime epoch)
              (posix-localtime epoch))
          (make-date sec min hour mday mon (+ year 1900)
                     (or tz-name "")
                     gmtoff
                     (not (zero? isdst))
                     wday yday)))))

  ;; ======================================================================
  ;; Date formatting
  ;; ======================================================================

  ;; Convert scsh format directives (~X) to strftime directives (%X).
  ;; scsh uses ~ prefix; strftime uses % prefix.
  (define (scsh-fmt->strftime-fmt fmt)
    (let ([len (string-length fmt)])
      (let lp ([i 0] [acc '()])
        (if (= i len)
            (list->string (reverse acc))
            (let ([c (string-ref fmt i)])
              (cond
                [(and (char=? c #\~) (< (+ i 1) len))
                 ;; Replace ~ with %
                 (lp (+ i 2) (cons (string-ref fmt (+ i 1)) (cons #\% acc)))]
                [(char=? c #\%)
                 ;; Escape literal % as %%
                 (lp (+ i 1) (cons #\% (cons #\% acc)))]
                [else
                 (lp (+ i 1) (cons c acc))]))))))

  ;; format-date: format a date record using scsh-style format directives.
  ;; Uses ~ prefix for directives (like scsh), converted to strftime's % prefix.
  ;; Common directives: ~Y=year, ~m=month, ~d=day, ~H=hour, ~M=minute, ~S=second,
  ;; ~a=abbrev weekday, ~b=abbrev month, ~Z=timezone
  (define (format-date fmt d)
    (check-arg date? d format-date)
    (let ([strfmt (scsh-fmt->strftime-fmt fmt)])
      (posix-strftime strfmt
                      (date:seconds d)
                      (date:minute d)
                      (date:hour d)
                      (date:month-day d)
                      (date:month d)
                      (- (date:year d) 1900)
                      (date:week-day d)
                      (date:year-day d)
                      (if (date:summer? d) 1 0)
                      (or (date:tz-secs d) 0)
                      (or (date:tz-name d) ""))))

  ;; date->string: format date as "Sun Sep 16 01:03:52 1973" (ctime-style).
  (define (date->string d)
    (format-date "~a ~b ~d ~H:~M:~S ~Y" d))

  ;; ======================================================================
  ;; Sub-second time resolution
  ;; ======================================================================

  ;; ticks/sec: returns the number of ticks per second.
  ;; We use microsecond resolution from gettimeofday, so 1000000.
  (define (ticks/sec) 1000000)

  ;; time+ticks: returns (values epoch-seconds microsecond-ticks).
  ;; Uses gettimeofday for sub-second precision.
  (define (time+ticks)
    (posix-gettimeofday))

) ;; end library
