;;; (hafod internal posix-time) -- Time, login, process times, sysconf, gettimeofday
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal posix-time)
  (export
    posix-localtime posix-gmtime posix-mktime posix-strftime posix-time
    posix-getlogin posix-times posix-sysconf posix-gettimeofday)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants)
          (hafod internal platform-constants) (hafod internal posix-core))

  ;; ======================================================================
  ;; Time -- localtime_r, gmtime_r, mktime, strftime, time
  ;; ======================================================================

  (define c-localtime_r (foreign-procedure "localtime_r" (void* void*) void*))
  (define c-gmtime_r (foreign-procedure "gmtime_r" (void* void*) void*))
  (define c-mktime (foreign-procedure "mktime" (void*) long))
  (define c-strftime (foreign-procedure "strftime" (void* size_t string void*) size_t))
  (define c-time (foreign-procedure "time" (void*) long))

  ;; posix-time: return current epoch seconds.
  (define (posix-time)
    (c-time 0))

  ;; Fill a struct tm buffer from individual fields.
  (define (fill-tm! buf sec min hour mday mon year isdst)
    (foreign-set! 'int buf TM-SEC sec)
    (foreign-set! 'int buf TM-MIN min)
    (foreign-set! 'int buf TM-HOUR hour)
    (foreign-set! 'int buf TM-MDAY mday)
    (foreign-set! 'int buf TM-MON mon)
    (foreign-set! 'int buf TM-YEAR year)
    (foreign-set! 'int buf TM-ISDST isdst))

  ;; Extract fields from a struct tm buffer.
  ;; Returns: (values sec min hour mday mon year wday yday isdst gmtoff tz-name)
  (define (extract-tm buf)
    (values
      (foreign-ref 'int buf TM-SEC)
      (foreign-ref 'int buf TM-MIN)
      (foreign-ref 'int buf TM-HOUR)
      (foreign-ref 'int buf TM-MDAY)
      (foreign-ref 'int buf TM-MON)
      (foreign-ref 'int buf TM-YEAR)
      (foreign-ref 'int buf TM-WDAY)
      (foreign-ref 'int buf TM-YDAY)
      (foreign-ref 'int buf TM-ISDST)
      (foreign-ref 'long buf TM-GMTOFF)
      (let ([zone-ptr (foreign-ref 'uptr buf TM-ZONE)])
        (if (= zone-ptr 0) #f (ptr->string zone-ptr)))))

  ;; posix-localtime: convert epoch seconds to local time fields.
  ;; Returns: (values sec min hour mday mon year wday yday isdst gmtoff tz-name)
  (define (posix-localtime epoch)
    (with-foreign-buffer ([time-buf 8] [tm-buf SIZEOF-TM])
      (foreign-set! 'long time-buf 0 epoch)
      (let ([result (c-localtime_r time-buf tm-buf)])
        (when (= result 0)
          (error 'posix-localtime "localtime_r failed" epoch))
        (extract-tm tm-buf))))

  ;; posix-gmtime: convert epoch seconds to UTC time fields.
  ;; Returns: (values sec min hour mday mon year wday yday isdst gmtoff tz-name)
  (define (posix-gmtime epoch)
    (with-foreign-buffer ([time-buf 8] [tm-buf SIZEOF-TM])
      (foreign-set! 'long time-buf 0 epoch)
      (let ([result (c-gmtime_r time-buf tm-buf)])
        (when (= result 0)
          (error 'posix-gmtime "gmtime_r failed" epoch))
        (extract-tm tm-buf))))

  ;; posix-mktime: convert struct tm fields to epoch seconds.
  ;; Takes same fields as fill-tm!. Returns epoch seconds or raises error.
  (define (posix-mktime sec min hour mday mon year isdst)
    (with-foreign-buffer ([tm-buf SIZEOF-TM])
      ;; Zero out the buffer first
      (do ([i 0 (+ i 1)]) ((= i SIZEOF-TM))
        (foreign-set! 'unsigned-8 tm-buf i 0))
      (fill-tm! tm-buf sec min hour mday mon year isdst)
      (let ([result (c-mktime tm-buf)])
        (when (= result -1)
          (error 'posix-mktime "mktime failed"))
        result)))

  ;; posix-strftime: format a date/time using strftime(3).
  ;; Takes a format string and struct tm field values.
  ;; Returns a formatted string.
  (define (posix-strftime fmt sec min hour mday mon year wday yday isdst gmtoff tz-name)
    (with-foreign-buffer ([tm-buf SIZEOF-TM] [out-buf 256])
      ;; Zero out the tm buffer
      (do ([i 0 (+ i 1)]) ((= i SIZEOF-TM))
        (foreign-set! 'unsigned-8 tm-buf i 0))
      (foreign-set! 'int tm-buf TM-SEC sec)
      (foreign-set! 'int tm-buf TM-MIN min)
      (foreign-set! 'int tm-buf TM-HOUR hour)
      (foreign-set! 'int tm-buf TM-MDAY mday)
      (foreign-set! 'int tm-buf TM-MON mon)
      (foreign-set! 'int tm-buf TM-YEAR year)
      (foreign-set! 'int tm-buf TM-WDAY wday)
      (foreign-set! 'int tm-buf TM-YDAY yday)
      (foreign-set! 'int tm-buf TM-ISDST isdst)
      (foreign-set! 'long tm-buf TM-GMTOFF gmtoff)
      ;; Note: tm_zone left as NULL -- strftime usually doesn't need it
      (let ([n (c-strftime out-buf 255 fmt tm-buf)])
        (if (zero? n)
            ""  ;; strftime returns 0 on error or empty result
            (ptr->string out-buf)))))

  ;; ======================================================================
  ;; Login name (getlogin)
  ;; ======================================================================

  (define c-getlogin (foreign-procedure "getlogin" () void*))

  ;; posix-getlogin: returns the login name of the user, or #f if unavailable.
  (define (posix-getlogin)
    (let ([ptr (c-getlogin)])
      (if (= ptr 0) #f (ptr->string ptr))))

  ;; ======================================================================
  ;; Process times (times(2))
  ;; ======================================================================

  (define c-times (foreign-procedure "times" (void*) long))

  ;; posix-times: returns (values utime stime cutime cstime) in clock ticks.
  (define (posix-times)
    (with-foreign-buffer ([buf SIZEOF-TMS])
      (let ([ret (c-times buf)])
        (when (= ret -1)
          (let ([err (foreign-ref 'int (__errno_location) 0)])
            (raise-posix-error 'times err)))
        (values
          (foreign-ref 'long buf 0)    ;; tms_utime
          (foreign-ref 'long buf 8)    ;; tms_stime
          (foreign-ref 'long buf 16)   ;; tms_cutime
          (foreign-ref 'long buf 24))))) ;; tms_cstime

  ;; ======================================================================
  ;; sysconf(3)
  ;; ======================================================================

  (define c-sysconf (foreign-procedure "sysconf" (int) long))

  ;; posix-sysconf: query system configuration.
  ;; Common constants: _SC_CLK_TCK = 2 on Linux
  (define (posix-sysconf name)
    (let ([ret (c-sysconf name)])
      (when (= ret -1)
        (error 'posix-sysconf "sysconf failed for" name))
      ret))

  ;; ======================================================================
  ;; gettimeofday(2) -- sub-second time resolution
  ;; ======================================================================

  ;; struct timeval: tv_sec (long, 8 bytes) + tv_usec (long, 8 bytes) = 16 bytes
  (define c-gettimeofday (foreign-procedure "gettimeofday" (void* void*) int))

  ;; posix-gettimeofday: returns (values seconds microseconds).
  (define (posix-gettimeofday)
    (with-foreign-buffer ([buf 16])
      (let ([ret (c-gettimeofday buf 0)])
        (when (< ret 0)
          (let ([err (foreign-ref 'int (__errno_location) 0)])
            (raise-posix-error 'gettimeofday err)))
        (values
          (foreign-ref 'long buf 0)    ;; tv_sec
          (foreign-ref 'long buf 8))))) ;; tv_usec

  ) ; end library
