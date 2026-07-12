;;; (hafod temp-file) -- Temporary file creation
;;; create-temp-file, temp-file-channel, temp-file-iterate
;;; Ported from scsh/scheme/temp-file.scm

(library (hafod temp-file)
  (export create-temp-file temp-file-channel temp-file-iterate
          *temp-file-template*)

  (import (hafod internal base)
          (hafod posix) (hafod compat) (hafod fd-ports) (hafod environment))

  ;; Build the temp directory path, respecting $TMPDIR.
  (define (temp-dir)
    (or (getenv "TMPDIR") "/tmp"))

  ;; *temp-file-template*: parameter for default temp file name prefix.
  (define *temp-file-template*
    (make-parameter (string-append (temp-dir) "/hafod-tmp-")))

  ;; create-temp-file: create a temporary file and return its path.
  ;; The file is created with mode 0600 (owner read/write only) via mkstemp.
  ;; The caller is responsible for deleting the file when done.
  (define (create-temp-file . maybe-prefix)
    (let* ([prefix (:optional maybe-prefix (*temp-file-template*))]
           [template (string-append prefix "XXXXXX")])
      (receive (path fd) (posix-mkstemp template)
        (posix-close fd)
        path)))

  ;; Unlink a temp file on an error path, tolerating its prior removal.
  ;; Mirrors the ENOENT-tolerant cleanup in (hafod fileinfo): swallow "no such
  ;; file" (the file was never created, or already unlinked) and re-raise any
  ;; other error.
  (define (unlink-temp-file path)
    (guard (e [(posix-error? e)
               (unless (= (posix-errno e) 2) (raise e))])  ; ENOENT=2 -> already gone
      (posix-unlink path)))

  ;; temp-file-channel: create a temp file, unlink it, return (values inport outport).
  ;; The file has no name on the filesystem but stays alive via open fds.
  ;; This is used by run/collecting* to buffer multi-fd output without deadlock.
  (define (temp-file-channel)
    (let ([template (string-append (temp-dir) "/hafod-chan-XXXXXX")])
      (receive (path fd) (posix-mkstemp template)
        ;; Stage the cleanup of the window between mkstemp and the successful
        ;; return.  If any step raises, release the descriptor's current owner
        ;; -- the raw fd before the port wraps it, otherwise the port (closing
        ;; the raw fd as well would double-close a port-owned descriptor) -- and
        ;; unlink the temp path, then re-raise.  The success path is unchanged:
        ;; the file is unlinked exactly once and (values inport outport) returned.
        (let ([oport #f])
          (guard (e [#t
                     (if oport
                         (guard (inner [#t #f]) (close oport))
                         (guard (inner [#t #f]) (posix-close fd)))
                     (unlink-temp-file path)
                     (raise e)])
            (set! oport (fdes->outport fd))
            (let ([iport (open-file path open/read)])
              ;; Unlink immediately -- file stays alive via open fds
              (posix-unlink path)
              (values iport oport)))))))

  ;; temp-file-iterate: try candidate names until maker succeeds.
  ;; MAKER is called with a filename string and should either return a value
  ;; (success) or raise an exception (try next name).
  ;; TEMPLATE should contain ~a which is replaced with the iteration number.
  ;; Defaults to "${TMPDIR}/hafod-<pid>.~a"
  (define (temp-file-iterate maker . maybe-template)
    (let ([template (:optional maybe-template
                      (string-append (temp-dir) "/hafod-"
                                     (number->string (posix-getpid))
                                     ".~a"))])
      (let loop ([i 0])
        (when (> i 1000)
          (error 'temp-file-iterate "Cannot create temp file after 1000 attempts"))
        (let ([fname (let* ([num-str (number->string i)]
                            [tpl-len (string-length template)]
                            ;; Find ~a in template and replace
                            [result (let scan ([j 0] [acc '()])
                                      (cond
                                        [(>= j tpl-len)
                                         (list->string (reverse acc))]
                                        [(and (< (+ j 1) tpl-len)
                                              (char=? (string-ref template j) #\~)
                                              (char=? (string-ref template (+ j 1)) #\a))
                                         (scan (+ j 2)
                                               (append (reverse (string->list num-str)) acc))]
                                        [else
                                         (scan (+ j 1) (cons (string-ref template j) acc))]))])
                       result)])
          (guard (e [#t (loop (+ i 1))])
            (maker fname))))))

) ;; end library
