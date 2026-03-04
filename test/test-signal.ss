(library-directories '(("src" . "src") ("." . ".")))
(import (test runner) (hafod signal) (hafod posix))

(test-begin "signal")

;; Signal constants are positive integers
(test-assert "SIGTERM is positive integer" (and (integer? SIGTERM) (> SIGTERM 0)))
(test-assert "SIGINT is positive integer" (and (integer? SIGINT) (> SIGINT 0)))
(test-assert "SIGKILL is positive integer" (and (integer? SIGKILL) (> SIGKILL 0)))

;; signal-process with signal 0 (existence check)
(test-assert "signal-process self with signal 0"
  (begin (signal-process (posix-getpid) 0) #t))

;; signal-process-group with signal 0
;; Use the actual process group ID, not pid
(test-assert "signal-process-group self with signal 0"
  (begin (signal-process-group (posix-getpgrp) 0) #t))

;; signal-process with bad pid should raise posix error
(test-error "signal-process bad pid raises error"
  (signal-process 999999 SIGTERM))

;; signal-process with non-integer should raise error
(test-error "signal-process non-integer raises error"
  (signal-process "bad" 0))

;; signal-process-group with non-integer should raise error
(test-error "signal-process-group non-integer raises error"
  (signal-process-group "bad" 0))

(test-end)
