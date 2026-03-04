;;; Gap-fill tests for untested constant/flag symbols
;;; Phase 24 Plan 01 Task 1
;;; Copyright (c) 2026 Dominic Pearson.
(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod posix)
        (hafod signal)
        (hafod tty)
        (hafod compat)
        (except (chezscheme) vector-append open-input-file open-output-file getenv))

(test-begin "coverage-constants")

;; ======================================================================
;; Signal constants (16 symbols)
;; ======================================================================
(test-assert "SIGABRT is integer" (integer? SIGABRT))
(test-assert "SIGALRM is integer" (integer? SIGALRM))
(test-assert "SIGBUS is integer" (integer? SIGBUS))
(test-assert "SIGCHLD is integer" (integer? SIGCHLD))
(test-assert "SIGCONT is integer" (integer? SIGCONT))
(test-assert "SIGFPE is integer" (integer? SIGFPE))
(test-assert "SIGILL is integer" (integer? SIGILL))
(test-assert "SIGPIPE is integer" (integer? SIGPIPE))
(test-assert "SIGQUIT is integer" (integer? SIGQUIT))
(test-assert "SIGSEGV is integer" (integer? SIGSEGV))
(test-assert "SIGSTOP is integer" (integer? SIGSTOP))
(test-assert "SIGTRAP is integer" (integer? SIGTRAP))
(test-assert "SIGTSTP is integer" (integer? SIGTSTP))
(test-assert "SIGTTIN is integer" (integer? SIGTTIN))
(test-assert "SIGTTOU is integer" (integer? SIGTTOU))
(test-assert "SIGUSR2 is integer" (integer? SIGUSR2))

;; ======================================================================
;; POSIX constants (16 symbols)
;; ======================================================================
(test-assert "O_APPEND is integer" (integer? O_APPEND))
(test-assert "O_NONBLOCK is integer" (integer? O_NONBLOCK))
(test-assert "F_GETFL is integer" (integer? F_GETFL))
(test-assert "F_SETFL is integer" (integer? F_SETFL))
(test-assert "FNM_NOESCAPE is integer" (integer? FNM_NOESCAPE))
(test-assert "FNM_PATHNAME is integer" (integer? FNM_PATHNAME))
(test-assert "FNM_PERIOD is integer" (integer? FNM_PERIOD))
(test-assert "REG_NEWLINE is integer" (integer? REG_NEWLINE))
(test-assert "REG_NOMATCH is integer" (integer? REG_NOMATCH))
(test-assert "REG_NOSUB is integer" (integer? REG_NOSUB))
(test-assert "REG_NOTBOL is integer" (integer? REG_NOTBOL))
(test-assert "REG_NOTEOL is integer" (integer? REG_NOTEOL))
(test-assert "SEEK_CUR is integer" (integer? SEEK_CUR))
(test-assert "SEEK_END is integer" (integer? SEEK_END))
(test-assert "W_OK is integer" (integer? W_OK))
(test-assert "X_OK is integer" (integer? X_OK))

;; ======================================================================
;; S_IF* type constants (6 symbols)
;; ======================================================================
(test-assert "S_IFBLK is integer" (integer? S_IFBLK))
(test-assert "S_IFCHR is integer" (integer? S_IFCHR))
(test-assert "S_IFIFO is integer" (integer? S_IFIFO))
(test-assert "S_IFLNK is integer" (integer? S_IFLNK))
(test-assert "S_IFREG is integer" (integer? S_IFREG))
(test-assert "S_IFSOCK is integer" (integer? S_IFSOCK))

;; ======================================================================
;; S_I* permission constants (12 symbols)
;; ======================================================================
(test-assert "S_IRGRP is integer" (integer? S_IRGRP))
(test-assert "S_IROTH is integer" (integer? S_IROTH))
(test-assert "S_IRUSR is integer" (integer? S_IRUSR))
(test-assert "S_ISGID is integer" (integer? S_ISGID))
(test-assert "S_ISUID is integer" (integer? S_ISUID))
(test-assert "S_ISVTX is integer" (integer? S_ISVTX))
(test-assert "S_IWGRP is integer" (integer? S_IWGRP))
(test-assert "S_IWOTH is integer" (integer? S_IWOTH))
(test-assert "S_IWUSR is integer" (integer? S_IWUSR))
(test-assert "S_IXGRP is integer" (integer? S_IXGRP))
(test-assert "S_IXOTH is integer" (integer? S_IXOTH))
(test-assert "S_IXUSR is integer" (integer? S_IXUSR))

;; ======================================================================
;; POSIX stat-info record accessors (3 symbols)
;; ======================================================================
(let-values ([(path fd) (posix-mkstemp "/tmp/hafod-cov-const-XXXXXX")])
  (let ([si (posix-fstat fd)])
    (test-assert "stat-info-blksize returns integer" (integer? (stat-info-blksize si)))
    (test-assert "stat-info-blocks returns integer" (integer? (stat-info-blocks si)))
    (test-assert "stat-info-rdev returns integer" (integer? (stat-info-rdev si))))
  (posix-close fd)
  (posix-unlink path))

;; ======================================================================
;; POSIX TC* constants (10 symbols)
;; ======================================================================
(test-assert "TCIFLUSH is integer" (integer? TCIFLUSH))
(test-assert "TCIOFF is integer" (integer? TCIOFF))
(test-assert "TCIOFLUSH is integer" (integer? TCIOFLUSH))
(test-assert "TCION is integer" (integer? TCION))
(test-assert "TCOFLUSH is integer" (integer? TCOFLUSH))
(test-assert "TCOOFF is integer" (integer? TCOOFF))
(test-assert "TCOON is integer" (integer? TCOON))
(test-assert "TCSADRAIN is integer" (integer? TCSADRAIN))
(test-assert "TCSAFLUSH is integer" (integer? TCSAFLUSH))
(test-assert "TCSANOW is integer" (integer? TCSANOW))

;; ======================================================================
;; TTY control-flag constants (11 symbols)
;; Some may be #f on Linux (not available), test with or
;; ======================================================================
(test-assert "ttyc/2-stop-bits is integer or #f" (or (integer? ttyc/2-stop-bits) (not ttyc/2-stop-bits)))
(test-assert "ttyc/carrier-flow-ctl is integer or #f" (or (integer? ttyc/carrier-flow-ctl) (not ttyc/carrier-flow-ctl)))
(test-assert "ttyc/char-size5 is integer or #f" (or (integer? ttyc/char-size5) (not ttyc/char-size5)))
(test-assert "ttyc/char-size6 is integer or #f" (or (integer? ttyc/char-size6) (not ttyc/char-size6)))
(test-assert "ttyc/char-size7 is integer or #f" (or (integer? ttyc/char-size7) (not ttyc/char-size7)))
(test-assert "ttyc/CTS-output-flow-ctl is integer or #f" (or (integer? ttyc/CTS-output-flow-ctl) (not ttyc/CTS-output-flow-ctl)))
(test-assert "ttyc/enable-parity is integer or #f" (or (integer? ttyc/enable-parity) (not ttyc/enable-parity)))
(test-assert "ttyc/ignore-flags is integer or #f" (or (integer? ttyc/ignore-flags) (not ttyc/ignore-flags)))
(test-assert "ttyc/no-modem-sync is integer or #f" (or (integer? ttyc/no-modem-sync) (not ttyc/no-modem-sync)))
(test-assert "ttyc/odd-parity is integer or #f" (or (integer? ttyc/odd-parity) (not ttyc/odd-parity)))
(test-assert "ttyc/RTS-input-flow-ctl is integer or #f" (or (integer? ttyc/RTS-input-flow-ctl) (not ttyc/RTS-input-flow-ctl)))

;; ======================================================================
;; TTY input-flag constants (10 symbols)
;; ======================================================================
(test-assert "ttyin/7bits is integer or #f" (or (integer? ttyin/7bits) (not ttyin/7bits)))
(test-assert "ttyin/beep-on-overflow is integer or #f" (or (integer? ttyin/beep-on-overflow) (not ttyin/beep-on-overflow)))
(test-assert "ttyin/check-parity is integer or #f" (or (integer? ttyin/check-parity) (not ttyin/check-parity)))
(test-assert "ttyin/ignore-bad-parity-chars is integer or #f" (or (integer? ttyin/ignore-bad-parity-chars) (not ttyin/ignore-bad-parity-chars)))
(test-assert "ttyin/ignore-cr is integer or #f" (or (integer? ttyin/ignore-cr) (not ttyin/ignore-cr)))
(test-assert "ttyin/lowercase is integer or #f" (or (integer? ttyin/lowercase) (not ttyin/lowercase)))
(test-assert "ttyin/mark-parity-errors is integer or #f" (or (integer? ttyin/mark-parity-errors) (not ttyin/mark-parity-errors)))
(test-assert "ttyin/nl->cr is integer or #f" (or (integer? ttyin/nl->cr) (not ttyin/nl->cr)))
(test-assert "ttyin/xon-any is integer or #f" (or (integer? ttyin/xon-any) (not ttyin/xon-any)))

;; ======================================================================
;; TTY local-flag constants (13 symbols)
;; ======================================================================
(test-assert "ttyl/alt-delete-word is integer or #f" (or (integer? ttyl/alt-delete-word) (not ttyl/alt-delete-word)))
(test-assert "ttyl/case-map is integer or #f" (or (integer? ttyl/case-map) (not ttyl/case-map)))
(test-assert "ttyl/echo-ctl is integer or #f" (or (integer? ttyl/echo-ctl) (not ttyl/echo-ctl)))
(test-assert "ttyl/echo-delete-line is integer or #f" (or (integer? ttyl/echo-delete-line) (not ttyl/echo-delete-line)))
(test-assert "ttyl/echo-nl is integer or #f" (or (integer? ttyl/echo-nl) (not ttyl/echo-nl)))
(test-assert "ttyl/flush-output is integer or #f" (or (integer? ttyl/flush-output) (not ttyl/flush-output)))
(test-assert "ttyl/hardcopy-delete is integer or #f" (or (integer? ttyl/hardcopy-delete) (not ttyl/hardcopy-delete)))
(test-assert "ttyl/no-flush-on-interrupt is integer or #f" (or (integer? ttyl/no-flush-on-interrupt) (not ttyl/no-flush-on-interrupt)))
(test-assert "ttyl/no-kernel-status is integer or #f" (or (integer? ttyl/no-kernel-status) (not ttyl/no-kernel-status)))
(test-assert "ttyl/reprint-unread-chars is integer or #f" (or (integer? ttyl/reprint-unread-chars) (not ttyl/reprint-unread-chars)))
(test-assert "ttyl/ttou-signal is integer or #f" (or (integer? ttyl/ttou-signal) (not ttyl/ttou-signal)))
(test-assert "ttyl/visual-delete is integer or #f" (or (integer? ttyl/visual-delete) (not ttyl/visual-delete)))
(test-assert "ttyl/visual-delete-line is integer or #f" (or (integer? ttyl/visual-delete-line) (not ttyl/visual-delete-line)))

;; ======================================================================
;; TTY output-flag constants (24 symbols)
;; ======================================================================
(test-assert "ttyout/bs-delay0 is integer or #f" (or (integer? ttyout/bs-delay0) (not ttyout/bs-delay0)))
(test-assert "ttyout/bs-delay1 is integer or #f" (or (integer? ttyout/bs-delay1) (not ttyout/bs-delay1)))
(test-assert "ttyout/cr-delay0 is integer or #f" (or (integer? ttyout/cr-delay0) (not ttyout/cr-delay0)))
(test-assert "ttyout/cr-delay1 is integer or #f" (or (integer? ttyout/cr-delay1) (not ttyout/cr-delay1)))
(test-assert "ttyout/cr-delay2 is integer or #f" (or (integer? ttyout/cr-delay2) (not ttyout/cr-delay2)))
(test-assert "ttyout/cr-delay3 is integer or #f" (or (integer? ttyout/cr-delay3) (not ttyout/cr-delay3)))
(test-assert "ttyout/cr->nl is integer or #f" (or (integer? ttyout/cr->nl) (not ttyout/cr->nl)))
(test-assert "ttyout/delay-w/fill-char is integer or #f" (or (integer? ttyout/delay-w/fill-char) (not ttyout/delay-w/fill-char)))
(test-assert "ttyout/discard-eot is integer or #f" (or (integer? ttyout/discard-eot) (not ttyout/discard-eot)))
(test-assert "ttyout/expand-tabs is integer or #f" (or (integer? ttyout/expand-tabs) (not ttyout/expand-tabs)))
(test-assert "ttyout/ff-delay0 is integer or #f" (or (integer? ttyout/ff-delay0) (not ttyout/ff-delay0)))
(test-assert "ttyout/ff-delay1 is integer or #f" (or (integer? ttyout/ff-delay1) (not ttyout/ff-delay1)))
(test-assert "ttyout/fill-w/del is integer or #f" (or (integer? ttyout/fill-w/del) (not ttyout/fill-w/del)))
(test-assert "ttyout/nl-delay0 is integer or #f" (or (integer? ttyout/nl-delay0) (not ttyout/nl-delay0)))
(test-assert "ttyout/nl-delay1 is integer or #f" (or (integer? ttyout/nl-delay1) (not ttyout/nl-delay1)))
(test-assert "ttyout/nl-does-cr is integer or #f" (or (integer? ttyout/nl-does-cr) (not ttyout/nl-does-cr)))
(test-assert "ttyout/no-col0-cr is integer or #f" (or (integer? ttyout/no-col0-cr) (not ttyout/no-col0-cr)))
(test-assert "ttyout/tab-delay0 is integer or #f" (or (integer? ttyout/tab-delay0) (not ttyout/tab-delay0)))
(test-assert "ttyout/tab-delay1 is integer or #f" (or (integer? ttyout/tab-delay1) (not ttyout/tab-delay1)))
(test-assert "ttyout/tab-delay2 is integer or #f" (or (integer? ttyout/tab-delay2) (not ttyout/tab-delay2)))
(test-assert "ttyout/tab-delayx is integer or #f" (or (integer? ttyout/tab-delayx) (not ttyout/tab-delayx)))
(test-assert "ttyout/uppercase is integer or #f" (or (integer? ttyout/uppercase) (not ttyout/uppercase)))
(test-assert "ttyout/vtab-delay0 is integer or #f" (or (integer? ttyout/vtab-delay0) (not ttyout/vtab-delay0)))
(test-assert "ttyout/vtab-delay1 is integer or #f" (or (integer? ttyout/vtab-delay1) (not ttyout/vtab-delay1)))

(test-end)
