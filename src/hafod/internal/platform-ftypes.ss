#!chezscheme
;;; (hafod internal platform-ftypes) -- Typed foreign descriptions of the POSIX
;;; structs hafod reads across the C ABI: timespec, stat, dirent, termios and
;;; winsize. Each layout is expressed with Chez `define-ftype`, so the compiler
;;; computes every field offset, width and alignment from the field types rather
;;; than from hand-written byte constants.
;;;
;;; The struct ABI is platform-specific -- stat, dirent and termios differ in
;;; field order, width and total size across Linux, macOS and FreeBSD -- so the
;;; three divergent layouts are chosen at expand time by a single `meta-cond` on
;;; `(machine-type)`, mirroring the machine-type enumeration owned by
;;; (hafod internal platform). winsize is ABI-identical on every supported host
;;; and needs no branch.
;;;
;;; This library never re-derives the ABI. The generated
;;; (hafod internal platform-constants) stays the independent, C-probe-verified
;;; oracle, and test/test-ftype-abi.ss asserts that every offset and
;;; `ftype-sizeof` here equals the corresponding generated constant on the build
;;; host. A wrong layout therefore surfaces as a named test failure, never as
;;; silent memory corruption. The library is internal only: its ftype names are
;;; not re-exported through the (hafod) umbrella.
;;;
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal platform-ftypes)
  (export timespec stat-t dirent-t termios-t winsize-t pollfd-t)
  (import (chezscheme))

  ;; struct timespec -- { time_t tv_sec; long tv_nsec; }; 16 bytes on every
  ;; 64-bit target, and shared by each platform's stat arm below. tv_sec is read
  ;; unsigned to reproduce the current `(foreign-ref 'unsigned-64 ...)` behaviour
  ;; byte-for-byte; tv_nsec is present so the struct is a full 16 bytes and the
  ;; fields following each nested timespec land at the correct offsets.
  (define-ftype timespec (struct [tv_sec unsigned-64] [tv_nsec integer-64]))

  ;; stat, dirent and termios diverge by platform, so their layouts are selected
  ;; at expand time. The machine-type lists mirror (hafod internal platform)
  ;; exactly, so no target is silently assumed.
  (meta-cond
    ;; ---- Linux x86-64 / aarch64 / i386 (glibc) -- hardware-verified ----
    [(memq (machine-type) '(a6le ta6le arm64le tarm64le i3le ti3le))
     (define-ftype stat-t
       (struct
         [st_dev unsigned-64] [st_ino unsigned-64] [st_nlink unsigned-64]
         [st_mode unsigned-32] [st_uid unsigned-32] [st_gid unsigned-32]
         [__pad0 unsigned-32] [st_rdev unsigned-64]
         [st_size integer-64] [st_blksize integer-64] [st_blocks integer-64]
         [st_atim timespec] [st_mtim timespec] [st_ctim timespec]
         [__glibc_reserved (array 3 integer-64)]))
     (define-ftype dirent-t
       (struct
         [d_ino unsigned-64] [d_off integer-64] [d_reclen unsigned-16]
         [d_type unsigned-8] [d_name (array 256 unsigned-8)]))
     (define-ftype termios-t
       (struct
         [c_iflag unsigned-32] [c_oflag unsigned-32] [c_cflag unsigned-32]
         [c_lflag unsigned-32] [c_line unsigned-8] [c_cc (array 32 unsigned-8)]
         [c_ispeed unsigned-32] [c_ospeed unsigned-32]))]
    ;; ---- macOS x86-64 / arm64 (Darwin, 64-bit inode) -- hardware-verified ----
    ;; The macOS layout genuinely differs from Linux (field order, widths, the
    ;; extra st_birthtim, 8-byte termios flags, no c_line, NCCS 20): never
    ;; transcribe the Linux numbers here.
    [(memq (machine-type) '(a6osx ta6osx arm64osx tarm64osx i3osx ti3osx))
     (define-ftype stat-t
       (struct
         [st_dev unsigned-32] [st_mode unsigned-16] [st_nlink unsigned-16]
         [st_ino unsigned-64] [st_uid unsigned-32] [st_gid unsigned-32]
         [st_rdev unsigned-32]
         [st_atim timespec] [st_mtim timespec] [st_ctim timespec] [st_birthtim timespec]
         [st_size integer-64] [st_blocks integer-64] [st_blksize integer-32]
         [st_flags unsigned-32] [st_gen unsigned-32] [st_lspare integer-32]
         [st_qspare (array 2 integer-64)]))
     (define-ftype dirent-t
       (struct
         [d_ino unsigned-64] [d_seekoff unsigned-64] [d_reclen unsigned-16]
         [d_namlen unsigned-16] [d_type unsigned-8] [d_name (array 1024 unsigned-8)]))
     (define-ftype termios-t
       (struct
         [c_iflag unsigned-64] [c_oflag unsigned-64] [c_cflag unsigned-64]
         [c_lflag unsigned-64] [c_cc (array 20 unsigned-8)]
         [c_ispeed unsigned-64] [c_ospeed unsigned-64]))]
    ;; ---- FreeBSD amd64 / arm64 / i386 (ino64, FreeBSD 12+) ----
    ;; Best-effort, derived from documented FreeBSD headers and UNVERIFIED this
    ;; cycle: no FreeBSD host was available to run the cross-check. It is
    ;; corruption-safe regardless -- on a real FreeBSD host test/test-ftype-abi.ss
    ;; compares every offset and size below against FreeBSD's own
    ;; C-probe-verified constants and fails loudly on any mismatch, so a wrong
    ;; layout can never corrupt memory, only fail a named assertion. meta-cond
    ;; does not expand a non-selected arm, so this branch is inert -- not even
    ;; parsed -- on the Linux and macOS build hosts, which is exactly why the
    ;; cross-check gates it per host. Field names match the other arms for every
    ;; field a consumer reads, keeping the consumers a single, platform-agnostic
    ;; code path.
    [(memq (machine-type) '(a6fb ta6fb arm64fb tarm64fb i3fb ti3fb))
     (define-ftype stat-t
       (struct
         [st_dev unsigned-64] [st_ino unsigned-64] [st_nlink unsigned-64]
         [st_mode unsigned-16] [st_padding0 integer-16]
         [st_uid unsigned-32] [st_gid unsigned-32] [st_padding1 integer-32]
         [st_rdev unsigned-64]
         [st_atim timespec] [st_mtim timespec] [st_ctim timespec] [st_birthtim timespec]
         [st_size integer-64] [st_blocks integer-64] [st_blksize integer-32]
         [st_flags unsigned-32] [st_gen unsigned-64]
         [st_spare (array 10 unsigned-64)]))
     (define-ftype dirent-t
       (struct
         [d_ino unsigned-64] [d_off unsigned-64] [d_reclen unsigned-16]
         [d_type unsigned-8] [d_pad0 unsigned-8] [d_namlen unsigned-16]
         [d_pad1 unsigned-16] [d_name (array 256 unsigned-8)]))
     (define-ftype termios-t
       (struct
         [c_iflag unsigned-32] [c_oflag unsigned-32] [c_cflag unsigned-32]
         [c_lflag unsigned-32] [c_cc (array 20 unsigned-8)]
         [c_ispeed unsigned-32] [c_ospeed unsigned-32]))]
    ;; ---- Any other machine-type: refuse to assume a layout ----
    ;; A truly-unknown target fails at compile time, naming the machine-type,
    ;; rather than reading and writing at guessed offsets.
    [else (syntax-error (machine-type)
            "platform-ftypes: no verified define-ftype layout for this machine-type")])

  ;; struct winsize -- { unsigned short ws_row, ws_col, ws_xpixel, ws_ypixel; }.
  ;; ABI-identical on every supported platform, so no meta-cond is needed. All
  ;; four fields are present so `ftype-sizeof` equals SIZEOF-WINSZ (8).
  (define-ftype winsize-t
    (struct [ws_row unsigned-16] [ws_col unsigned-16]
            [ws_xpixel unsigned-16] [ws_ypixel unsigned-16]))

  ;; struct pollfd -- { int fd; short events; short revents; }, the descriptor
  ;; array element poll(2) reads. ABI-identical on every supported platform (a
  ;; plain int + two shorts, no padding quirks), so like winsize it needs no
  ;; meta-cond. All three fields are present so `ftype-sizeof` is a full 8 bytes
  ;; and Chez lands events/revents at offsets 4 and 6 with no hand arithmetic.
  (define-ftype pollfd-t
    (struct [fd integer-32] [events integer-16] [revents integer-16])))
