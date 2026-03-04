;;; 01-system-info.ss -- Display system information
;;;
;;; Demonstrates: uname, process state, user/group database, time/date,
;;;               environment variables, file info
;;;
;;; Based on typical scsh system introspection scripts.
;;; Source: scsh manual sections on process state, user/group, time.

(import (hafod))

(define (hr)
  (display (make-string 60 #\-))
  (newline))

(define (field label value)
  (display (string-append "  " label ": "))
  (display value)
  (newline))

;;; System information
(let ([u (uname)])
  (display "=== System Information ===\n")
  (field "OS"       (uname:os-name u))
  (field "Hostname" (uname:node-name u))
  (field "Release"  (uname:release u))
  (field "Version"  (uname:version u))
  (field "Machine"  (uname:machine u)))
(hr)

;;; Process state
(display "=== Process State ===\n")
(field "PID"        (number->string (pid)))
(field "Parent PID" (number->string (parent-pid)))
(field "CWD"        (cwd))
(field "Umask"      (string-append "#o" (number->string (umask) 8)))
(field "UID"        (number->string (user-uid)))
(field "GID"        (number->string (user-gid)))
(field "EUID"       (number->string (user-effective-uid)))
(field "EGID"       (number->string (user-effective-gid)))
(field "Login"      (user-login-name))
(hr)

;;; User information from passwd database
(display "=== User Info ===\n")
(let ([ui (user-info (user-uid))])
  (field "Username"  (passwd-info-name ui))
  (field "Home"      (passwd-info-dir ui))
  (field "Shell"     (passwd-info-shell ui))
  (field "GECOS"     (passwd-info-gecos ui)))
(hr)

;;; Group information
(display "=== Group Info ===\n")
(let ([gi (group-info (user-gid))])
  (field "Primary group" (group-info-name gi))
  (field "GID"           (number->string (group-info-gid gi))))
(let ([supp (user-supplementary-gids)])
  (field "Supplementary" (apply string-append
                           (map (lambda (g)
                                  (string-append (number->string g) " "))
                                supp))))
(hr)

;;; Current time
(display "=== Date/Time ===\n")
(let ([d (date)])
  (field "Date"     (format-date "~Y-~m-~d" d))
  (field "Time"     (format-date "~H:~M:~S" d))
  (field "Timezone" (date:tz-name d))
  (field "Epoch"    (number->string (time))))
(hr)

;;; Key environment variables
(display "=== Environment (selected) ===\n")
(for-each
  (lambda (var)
    (let ([val (getenv var)])
      (when val (field var val))))
  '("HOME" "USER" "SHELL" "TERM" "LANG" "EDITOR" "DISPLAY"))
(hr)

;;; File info on home directory
(display "=== Home Directory Info ===\n")
(let* ([home (home-directory)]
       [info (file-info home)])
  (field "Path"     home)
  (field "Type"     (symbol->string (file-info:type info)))
  (field "Mode"     (string-append "#o" (number->string (file-info:mode info) 8)))
  (field "Size"     (number->string (file-info:size info)))
  (field "Links"    (number->string (file-info:nlinks info))))

(display "\nDone.\n")
