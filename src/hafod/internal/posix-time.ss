;;; (hafod internal posix-time) -- Time, login, process times, sysconf, gettimeofday
;;; Extracted from posix.ss during Phase 26 splitting.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal posix-time)
  (export
    posix-localtime posix-gmtime posix-mktime posix-strftime posix-time
    posix-getlogin posix-times posix-sysconf posix-gettimeofday)

  (import (chezscheme) (hafod internal errno) (hafod internal posix-constants) (hafod internal posix-core))

  (define load-libc (load-shared-object "libc.so.6"))

  ;; ======================================================================
  ;; Time -- localtime_r, gmtime_r, mktime, strftime, time
  ;; ======================================================================

  ;; struct tm layout (x86_64 Linux / glibc):
  ;; Offset  Type   Field
  ;; 0       int    tm_sec
  ;; 4       int    tm_min
  ;; 8       int    tm_hour
  ;; 12      int    tm_mday
  ;; 16      int    tm_mon    (0-11)
  ;; 20      int    tm_year   (years since 1900)
  ;; 24      int    tm_wday   (0=Sunday)
  ;; 28      int    tm_yday   (0-365)
  ;; 32      int    tm_isdst  (-1/0/1)
  ;; 40      long   tm_gmtoff (seconds east of UTC)
  ;; 48      char*  tm_zone
  ;; Total: 56 bytes

  (define SIZEOF_TM 56)

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
    (foreign-set! 'int buf 0 sec)
    (foreign-set! 'int buf 4 min)
    (foreign-set! 'int buf 8 hour)
    (foreign-set! 'int buf 12 mday)
    (foreign-set! 'int buf 16 mon)
    (foreign-set! 'int buf 20 year)
    (foreign-set! 'int buf 32 isdst))

  ;; Extract fields from a struct tm buffer.
  ;; Returns: (values sec min hour mday mon year wday yday isdst gmtoff tz-name)
  (define (extract-tm buf)
    (values
      (foreign-ref 'int buf 0)      ;; tm_sec
      (foreign-ref 'int buf 4)      ;; tm_min
      (foreign-ref 'int buf 8)      ;; tm_hour
      (foreign-ref 'int buf 12)     ;; tm_mday
      (foreign-ref 'int buf 16)     ;; tm_mon
      (foreign-ref 'int buf 20)     ;; tm_year
      (foreign-ref 'int buf 24)     ;; tm_wday
      (foreign-ref 'int buf 28)     ;; tm_yday
      (foreign-ref 'int buf 32)     ;; tm_isdst
      (foreign-ref 'long buf 40)    ;; tm_gmtoff
      (let ([zone-ptr (foreign-ref 'uptr buf 48)])  ;; tm_zone
        (if (= zone-ptr 0) #f (ptr->string zone-ptr)))))

  ;; posix-localtime: convert epoch seconds to local time fields.
  ;; Returns: (values sec min hour mday mon year wday yday isdst gmtoff tz-name)
  (define (posix-localtime epoch)
    (with-foreign-buffer ([time-buf 8] [tm-buf SIZEOF_TM])
      (foreign-set! 'long time-buf 0 epoch)
      (let ([result (c-localtime_r time-buf tm-buf)])
        (when (= result 0)
          (error 'posix-localtime "localtime_r failed" epoch))
        (extract-tm tm-buf))))

  ;; posix-gmtime: convert epoch seconds to UTC time fields.
  ;; Returns: (values sec min hour mday mon year wday yday isdst gmtoff tz-name)
  (define (posix-gmtime epoch)
    (with-foreign-buffer ([time-buf 8] [tm-buf SIZEOF_TM])
      (foreign-set! 'long time-buf 0 epoch)
      (let ([result (c-gmtime_r time-buf tm-buf)])
        (when (= result 0)
          (error 'posix-gmtime "gmtime_r failed" epoch))
        (extract-tm tm-buf))))

  ;; posix-mktime: convert struct tm fields to epoch seconds.
  ;; Takes same fields as fill-tm!. Returns epoch seconds or raises error.
  (define (posix-mktime sec min hour mday mon year isdst)
    (with-foreign-buffer ([tm-buf SIZEOF_TM])
      ;; Zero out the buffer first
      (do ([i 0 (+ i 1)]) ((= i SIZEOF_TM))
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
    (with-foreign-buffer ([tm-buf SIZEOF_TM] [out-buf 256])
      ;; Zero out the tm buffer
      (do ([i 0 (+ i 1)]) ((= i SIZEOF_TM))
        (foreign-set! 'unsigned-8 tm-buf i 0))
      (foreign-set! 'int tm-buf 0 sec)
      (foreign-set! 'int tm-buf 4 min)
      (foreign-set! 'int tm-buf 8 hour)
      (foreign-set! 'int tm-buf 12 mday)
      (foreign-set! 'int tm-buf 16 mon)
      (foreign-set! 'int tm-buf 20 year)
      (foreign-set! 'int tm-buf 24 wday)
      (foreign-set! 'int tm-buf 28 yday)
      (foreign-set! 'int tm-buf 32 isdst)
      (foreign-set! 'long tm-buf 40 gmtoff)
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

  ;; struct tms on x86_64 Linux: 4 fields of clock_t (long = 8 bytes each) = 32 bytes
  (define c-times (foreign-procedure "times" (void*) long))

  ;; posix-times: returns (values utime stime cutime cstime) in clock ticks.
  (define (posix-times)
    (with-foreign-buffer ([buf 32])
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
