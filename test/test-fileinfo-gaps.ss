;;; Tests for fileinfo gap filling: file-info predicates, file:* accessors,
;;; create-fifo, sync-file, sync-file-system
;;; Part of hafod v2.0 Phase 8 Plan 01

(import (chezscheme)
        (hafod fileinfo)
        (hafod fd-ports)
        (hafod posix)
        (hafod compat)
        (test runner))

(test-begin "fileinfo-gaps")

;;; ============================================================
;;; file-info-* type predicates (on stat-info records)
;;; ============================================================

(test-assert "file-info-directory? on /tmp returns #t"
  (file-info-directory? (file-info "/tmp")))

(test-assert "file-info-directory? on a regular file returns #f"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fitest-XXXXXX")])
    (posix-close fd)
    (let ([result (file-info-directory? (file-info path))])
      (posix-unlink path)
      (not result))))

(test-assert "file-info-regular? on a regular file returns #t"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fitest-XXXXXX")])
    (posix-close fd)
    (let ([result (file-info-regular? (file-info path))])
      (posix-unlink path)
      result)))

(test-assert "file-info-symlink? on a symlink returns #t"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fitest-XXXXXX")])
    (posix-close fd)
    (let ([link-path (string-append path "-link")])
      (posix-symlink path link-path)
      (let ([result (file-info-symlink? (file-info link-path #f))])
        (posix-unlink link-path)
        (posix-unlink path)
        result))))

(test-assert "file-info-fifo? on a fifo returns #t"
  (let ([path "/tmp/hafod-fifo-test"])
    (guard (e [#t #f]) (posix-unlink path))
    (posix-mkfifo path #o666)
    (let ([result (file-info-fifo? (file-info path))])
      (posix-unlink path)
      result)))

(test-assert "file-info-special? on regular file returns #f"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fitest-XXXXXX")])
    (posix-close fd)
    (let ([result (file-info-special? (file-info path))])
      (posix-unlink path)
      (not result))))

;;; ============================================================
;;; file-info permission predicates (on stat-info records)
;;; ============================================================

(test-assert "file-info-readable? on a readable temp file returns #t"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fiperm-XXXXXX")])
    (posix-close fd)
    (let ([result (file-info-readable? (file-info path))])
      (posix-unlink path)
      result)))

(test-assert "file-info-writable? on a writable temp file returns #t"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fiperm-XXXXXX")])
    (posix-close fd)
    (let ([result (file-info-writable? (file-info path))])
      (posix-unlink path)
      result)))

(test-assert "file-info-executable? on a non-executable temp file returns #f"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fiperm-XXXXXX")])
    (posix-close fd)
    ;; mkstemp creates with 0600 (rw for owner)
    (let ([result (file-info-executable? (file-info path))])
      (posix-unlink path)
      (not result))))

(test-assert "file-info-not-readable? is negation of file-info-readable?"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fiperm-XXXXXX")])
    (posix-close fd)
    (let* ([info (file-info path)]
           [r (file-info-readable? info)]
           [nr (file-info-not-readable? info)])
      (posix-unlink path)
      (not (eq? r nr)))))

(test-assert "file-info-not-writable? is negation of file-info-writable?"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fiperm-XXXXXX")])
    (posix-close fd)
    (let* ([info (file-info path)]
           [w (file-info-writable? info)]
           [nw (file-info-not-writable? info)])
      (posix-unlink path)
      (not (eq? w nw)))))

(test-assert "file-info-not-executable? is negation of file-info-executable?"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fiperm-XXXXXX")])
    (posix-close fd)
    (let* ([info (file-info path)]
           [x (file-info-executable? info)]
           [nx (file-info-not-executable? info)])
      (posix-unlink path)
      (not (eq? x nx)))))

;;; ============================================================
;;; Negated path predicates
;;; ============================================================

(test-assert "file-not-exists? on a nonexistent path returns #t"
  (file-not-exists? "/tmp/hafod-surely-does-not-exist-XXXXX"))

(test-assert "file-not-exists? on an existing path returns #f"
  (not (file-not-exists? "/tmp")))

(test-assert "file-not-readable? on /tmp returns #f (readable dir)"
  (not (file-not-readable? "/tmp")))

(test-assert "file-not-writable? on /tmp returns #f (writable dir)"
  (not (file-not-writable? "/tmp")))

(test-assert "file-not-executable? on /tmp returns #f (executable dir)"
  (not (file-not-executable? "/tmp")))

;;; ============================================================
;;; file:* shorthand accessors
;;; ============================================================

(test-equal "file:type on /tmp returns directory"
  'directory
  (file:type "/tmp"))

(test-assert "file:size on a file with known content"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fsize-XXXXXX")])
    (posix-write fd (string->utf8 "abcde"))
    (posix-close fd)
    (let ([sz (file:size path)])
      (posix-unlink path)
      (= sz 5))))

(test-assert "file:owner returns a non-negative integer"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fown-XXXXXX")])
    (posix-close fd)
    (let ([owner (file:owner path)])
      (posix-unlink path)
      (and (integer? owner) (>= owner 0)))))

(test-assert "file:group returns a non-negative integer"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fgrp-XXXXXX")])
    (posix-close fd)
    (let ([grp (file:group path)])
      (posix-unlink path)
      (and (integer? grp) (>= grp 0)))))

(test-assert "file:inode returns a positive integer"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fino-XXXXXX")])
    (posix-close fd)
    (let ([ino (file:inode path)])
      (posix-unlink path)
      (and (integer? ino) (> ino 0)))))

(test-assert "file:mode returns an integer"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fmod-XXXXXX")])
    (posix-close fd)
    (let ([mode (file:mode path)])
      (posix-unlink path)
      (integer? mode))))

(test-assert "file:nlinks returns a positive integer"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fnlk-XXXXXX")])
    (posix-close fd)
    (let ([nl (file:nlinks path)])
      (posix-unlink path)
      (and (integer? nl) (> nl 0)))))

(test-assert "file:last-access returns a non-negative integer"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-flat-XXXXXX")])
    (posix-close fd)
    (let ([at (file:last-access path)])
      (posix-unlink path)
      (and (integer? at) (>= at 0)))))

(test-assert "file:last-mod returns a non-negative integer"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-flmt-XXXXXX")])
    (posix-close fd)
    (let ([mt (file:last-mod path)])
      (posix-unlink path)
      (and (integer? mt) (>= mt 0)))))

(test-assert "file:last-status-change returns a non-negative integer"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-flsc-XXXXXX")])
    (posix-close fd)
    (let ([ct (file:last-status-change path)])
      (posix-unlink path)
      (and (integer? ct) (>= ct 0)))))

;;; ============================================================
;;; create-fifo
;;; ============================================================

(test-assert "create-fifo creates a fifo that file-fifo? returns #t for"
  (let ([path "/tmp/hafod-create-fifo-test"])
    (guard (e [#t #f]) (posix-unlink path))
    (create-fifo path)
    (let ([result (file-fifo? path)])
      (posix-unlink path)
      result)))

(test-assert "create-fifo with explicit mode"
  (let ([path "/tmp/hafod-create-fifo-mode-test"])
    (guard (e [#t #f]) (posix-unlink path))
    (create-fifo path #o644)
    (let ([result (file-fifo? path)])
      (posix-unlink path)
      result)))

;;; ============================================================
;;; sync-file and sync-file-system
;;; ============================================================

(test-assert "sync-file on an open fd does not error"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-sync-XXXXXX")])
    (posix-write fd (string->utf8 "data"))
    (sync-file fd)
    (posix-close fd)
    (posix-unlink path)
    #t))

(test-assert "sync-file on a port does not error"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-syncp-XXXXXX")])
    (posix-close fd)
    (let ([port (open-file path (bitwise-ior open/write open/create open/truncate))])
      (display "data" port)
      (flush-output-port port)
      (sync-file port)
      (close port)
      (posix-unlink path)
      #t)))

(test-assert "sync-file-system does not error"
  (begin (sync-file-system) #t))

;;; ============================================================
;;; Deprecated aliases
;;; ============================================================

(test-assert "file-attributes is same procedure as file-info"
  (eq? file-attributes file-info))

(test-assert "file-writeable? alias works on a writable file"
  (let-values ([(path fd) (posix-mkstemp "/tmp/hafod-fwrt-XXXXXX")])
    (posix-close fd)
    (let ([result (file-writeable? path)])
      (posix-unlink path)
      result)))

(test-end)
