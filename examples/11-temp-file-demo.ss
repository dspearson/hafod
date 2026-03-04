;;; 11-temp-file-demo.ss -- Temp file creation and I/O channels
;;;
;;; Demonstrates: create-temp-file, temp-file-channel,
;;;               temp-file-iterate, *temp-file-template*,
;;;               open-file, open-input-file, open-output-file,
;;;               port->string, close, delete-file, run/string
;;;
;;; Source: scsh manual temp-file section.

(import (hafod))

(define (section title)
  (newline)
  (display (string-append "=== " title " ===\n")))

;;; --- Basic temp file creation ---
(section "Basic Temp Files")

;; create-temp-file returns a unique filename
(let* ([tmp1 (create-temp-file)]
       [tmp2 (create-temp-file)])
  (display (string-append "Temp file 1: " tmp1 "\n"))
  (display (string-append "Temp file 2: " tmp2 "\n"))
  (display (string-append "Different? " (if (string=? tmp1 tmp2) "no!" "yes") "\n"))

  ;; Write to temp file, read back
  (let ([out (open-output-file tmp1)])
    (display "Hello from hafod temp file!\n" out)
    (display "Line 2\n" out)
    (close out))

  (let ([contents (run/string (cat ,tmp1))])
    (display (string-append "Contents:\n" contents)))

  ;; Clean up
  (delete-file tmp1)
  (delete-file tmp2))

;;; --- Temp file channel (anonymous pipe-like) ---
(section "Temp File Channel")

;; temp-file-channel returns two ports on an unlinked temp file.
;; The file is automatically deleted when both ports are closed.
(let-values ([(in out) (temp-file-channel)])
  (display "Writing to channel...\n")
  (display "Message through the channel\n" out)
  (display "Second message\n" out)
  (flush-output-port out)
  (close out)

  ;; Seek to beginning of the input side
  (display "Reading from channel...\n")
  (let ([data (port->string in)])
    (display (string-append "Got: " data)))
  (close in))

;;; --- Custom temp file template ---
(section "Custom Temp File Template")

(parameterize ([*temp-file-template* "/tmp/hafod-example-~a"])
  (let ([tmp (create-temp-file)])
    (display (string-append "Custom template file: " tmp "\n"))
    (delete-file tmp)))

;;; --- Using temp files for inter-process communication ---
(section "Temp Files for IPC")

;; Write output from a subprocess to a temp file, then process it
(let ([tmpfile (create-temp-file)])
  ;; Subprocess writes to the file
  (let ([port (open-output-file tmpfile)])
    (display (run/string (ls -la /tmp)) port)
    (close port))
  ;; Read and count lines
  (let* ([port (open-input-file tmpfile)]
         [lines (let loop ([acc '()])
                  (let ([line (read-line port)])
                    (if (eof-object? line)
                        (reverse acc)
                        (loop (cons line acc)))))])
    (close-input-port port)
    (display (string-append "ls -la /tmp produced "
                            (number->string (length lines)) " lines\n"))
    ;; Show first 5
    (display "First 5 lines:\n")
    (for-each
      (lambda (line) (display (string-append "  " line "\n")))
      (if (> (length lines) 5)
          (let loop ([n 5] [l lines] [a '()])
            (if (or (zero? n) (null? l)) (reverse a)
                (loop (- n 1) (cdr l) (cons (car l) a))))
          lines)))
  (delete-file tmpfile))

;;; --- Temp file with explicit open-file flags ---
(section "Open File Flags")

(let ([tmpfile (create-temp-file)])
  ;; Open with explicit flags: write, create, truncate
  (let ([port (open-file tmpfile (bitwise-ior open/write open/create open/truncate))])
    (display "Written with explicit flags\n" port)
    (close port))

  ;; Open read-only
  (let ([port (open-file tmpfile open/read)])
    (display (string-append "Read back: " (port->string port)))
    (close port))

  ;; Append mode
  (let ([port (open-file tmpfile (bitwise-ior open/write open/append))])
    (display "Appended line\n" port)
    (close port))

  ;; Verify append worked
  (let ([port (open-input-file tmpfile)])
    (let ([lines (port->string-list port)])
      (close-input-port port)
      (display (string-append "After append: " (number->string (length lines)) " lines\n"))))

  (delete-file tmpfile))

(display "\nDone.\n")
