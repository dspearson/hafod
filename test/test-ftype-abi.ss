;;; (test test-ftype-abi) -- ABI cross-check for the define-ftype struct layouts.
;;; Every field offset and every `ftype-sizeof` in (hafod internal
;;; platform-ftypes) must equal the corresponding C-probe-verified constant that
;;; (hafod internal platform-constants) generates for this build host.
;;;
;;; This is the corruption-safety net for the FFI struct layouts. The generated
;;; constants remain the independent oracle -- they are produced by a C probe and
;;; guarded by `make check-c-probe` -- and this suite proves the typed ftype
;;; layout agrees with them field by field. A wrong per-platform layout therefore
;;; fails here as a named assertion and can never reach a syscall as a mis-sized
;;; buffer or an out-of-bounds field access.
;;;
;;; The same generated constants are read on whatever host builds, so this single
;;; file gates the Linux arm on Linux, the macOS arm on macOS and (in future) the
;;; FreeBSD arm on FreeBSD -- the corruption-safety net for the unverified BSD
;;; arm. It is auto-discovered by the Makefile test-% rule; no Makefile edit is
;;; needed.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (chezscheme)
        (hafod internal platform-ftypes)
        (hafod internal platform-constants))

;; Byte offset of a field within an ftype, computed with no live instance:
;; taking the address of a field off a null base pointer is pure address
;; arithmetic and reads no memory. The result is the field's offset in bytes.
(define-syntax off
  (syntax-rules ()
    [(_ T f ...)
     (ftype-pointer-address (ftype-&ref T (f ...) (make-ftype-pointer T 0)))]))

(test-begin "ftype-abi")

;; ======================================================================
;; struct stat -- field offsets, the nested timespec bases, and the size
;; ======================================================================
;; The size assertion proves the trailing reserved/padding tail is present: an
;; undersized allocation handed to stat() would let the kernel write past the
;; block. On Linux the size is 144. The st_atim/st_mtim/st_ctim offsets are the
;; base of each nested timespec, which is also the offset of its tv_sec member.
(test-equal "stat-t st_dev offset"     STAT-ST-DEV     (off stat-t st_dev))
(test-equal "stat-t st_ino offset"     STAT-ST-INO     (off stat-t st_ino))
(test-equal "stat-t st_nlink offset"   STAT-ST-NLINK   (off stat-t st_nlink))
(test-equal "stat-t st_mode offset"    STAT-ST-MODE    (off stat-t st_mode))
(test-equal "stat-t st_uid offset"     STAT-ST-UID     (off stat-t st_uid))
(test-equal "stat-t st_gid offset"     STAT-ST-GID     (off stat-t st_gid))
(test-equal "stat-t st_rdev offset"    STAT-ST-RDEV    (off stat-t st_rdev))
(test-equal "stat-t st_size offset"    STAT-ST-SIZE    (off stat-t st_size))
(test-equal "stat-t st_blksize offset" STAT-ST-BLKSIZE (off stat-t st_blksize))
(test-equal "stat-t st_blocks offset"  STAT-ST-BLOCKS  (off stat-t st_blocks))
(test-equal "stat-t st_atim offset"    STAT-ST-ATIM    (off stat-t st_atim))
(test-equal "stat-t st_mtim offset"    STAT-ST-MTIM    (off stat-t st_mtim))
(test-equal "stat-t st_ctim offset"    STAT-ST-CTIM    (off stat-t st_ctim))
(test-equal "stat-t ftype-sizeof equals SIZEOF-STAT" SIZEOF-STAT (ftype-sizeof stat-t))

;; ======================================================================
;; struct dirent -- only d_name's offset is load-bearing (read via ptr->string).
;; dirent is libc-owned (readdir returns a pointer into the DIR stream), so hafod
;; never allocates it and there is no size to assert.
;; ======================================================================
(test-equal "dirent-t d_name offset"   DIRENT-D-NAME   (off dirent-t d_name))

;; ======================================================================
;; struct termios -- field offsets, the c_cc array base, and the size
;; ======================================================================
(test-equal "termios-t c_iflag offset"  TERMIOS-C-IFLAG  (off termios-t c_iflag))
(test-equal "termios-t c_oflag offset"  TERMIOS-C-OFLAG  (off termios-t c_oflag))
(test-equal "termios-t c_cflag offset"  TERMIOS-C-CFLAG  (off termios-t c_cflag))
(test-equal "termios-t c_lflag offset"  TERMIOS-C-LFLAG  (off termios-t c_lflag))
(test-equal "termios-t c_cc offset"     TERMIOS-C-CC     (off termios-t c_cc))
(test-equal "termios-t c_ispeed offset" TERMIOS-C-ISPEED (off termios-t c_ispeed))
(test-equal "termios-t c_ospeed offset" TERMIOS-C-OSPEED (off termios-t c_ospeed))
(test-equal "termios-t ftype-sizeof equals SIZEOF-TERMIOS" SIZEOF-TERMIOS (ftype-sizeof termios-t))

;; ======================================================================
;; struct winsize -- ABI-identical everywhere; assert both read fields and the
;; full size.
;; ======================================================================
(test-equal "winsize-t ws_row offset" WINSZ-WS-ROW (off winsize-t ws_row))
(test-equal "winsize-t ws_col offset" WINSZ-WS-COL (off winsize-t ws_col))
(test-equal "winsize-t ftype-sizeof equals SIZEOF-WINSZ" SIZEOF-WINSZ (ftype-sizeof winsize-t))

(test-end)
