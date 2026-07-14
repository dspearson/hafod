;;; (test test-inode64-symbol) -- Selection assertions for the stat/readdir
;;; 64-bit-inode symbol seam in (hafod internal platform).
;;;
;;; inode64-symbol is a pure procedure of (base-name-string, machine-type-symbol),
;;; so this suite drives BOTH branches -- the 64-bit-Intel-macOS variant and the
;;; bare symbol every other target keeps -- entirely on the Linux CI host. No real
;;; macOS machine is needed, and the variant-vs-bare decision is verified by the
;;; VALUE the seam returns rather than by grepping the source for a suffix.
;;;
;;; This suite is non-vacuous: against a naive always-bare seam (base returned for
;;; every machine-type) the 64-bit-Intel-macOS assertions in the first section
;;; fail, so it has teeth even though this host can never exhibit the runtime
;;; metadata fault the seam repairs.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod internal platform))

(test-begin "inode64-symbol")

;; ======================================================================
;; 64-bit Intel macOS -- the one target that gets the $INODE64 variant
;; ======================================================================
;; On a6osx/ta6osx the bare libc stat/lstat/fstat/readdir resolve to the LEGACY
;; 32-bit-inode functions, whose struct layout no longer matches the 64-bit-inode
;; readers; the asm-labelled `…$INODE64` symbols are the 64-bit-inode variants.
;; Both the threaded ("t"-prefixed) and non-threaded machine-types select it.

(test-equal "stat/a6osx selects the 64-bit-inode variant"
  "stat$INODE64"    (inode64-symbol "stat"    'a6osx))
(test-equal "stat/ta6osx selects the 64-bit-inode variant"
  "stat$INODE64"    (inode64-symbol "stat"    'ta6osx))
(test-equal "lstat/a6osx selects the 64-bit-inode variant"
  "lstat$INODE64"   (inode64-symbol "lstat"   'a6osx))
(test-equal "lstat/ta6osx selects the 64-bit-inode variant"
  "lstat$INODE64"   (inode64-symbol "lstat"   'ta6osx))
(test-equal "fstat/a6osx selects the 64-bit-inode variant"
  "fstat$INODE64"   (inode64-symbol "fstat"   'a6osx))
(test-equal "fstat/ta6osx selects the 64-bit-inode variant"
  "fstat$INODE64"   (inode64-symbol "fstat"   'ta6osx))
(test-equal "readdir/a6osx selects the 64-bit-inode variant"
  "readdir$INODE64" (inode64-symbol "readdir" 'a6osx))
(test-equal "readdir/ta6osx selects the 64-bit-inode variant"
  "readdir$INODE64" (inode64-symbol "readdir" 'ta6osx))

;; ======================================================================
;; Linux x86_64 -- the live build host keeps every bare symbol
;; ======================================================================
;; ta6le is this box's machine-type, so these four assertions prove the rerouted
;; bindings resolve the BARE libc symbol here -- byte-identical to today. Naming a
;; variant that does not exist on Linux would fail resolution when the binding
;; form is evaluated, so "bare on ta6le" is exactly the off-Darwin invariant.

(test-equal "stat/ta6le keeps the bare symbol"
  "stat"    (inode64-symbol "stat"    'ta6le))
(test-equal "lstat/ta6le keeps the bare symbol"
  "lstat"   (inode64-symbol "lstat"   'ta6le))
(test-equal "fstat/ta6le keeps the bare symbol"
  "fstat"   (inode64-symbol "fstat"   'ta6le))
(test-equal "readdir/ta6le keeps the bare symbol"
  "readdir" (inode64-symbol "readdir" 'ta6le))
(test-equal "fstat/a6le keeps the bare symbol"
  "fstat"   (inode64-symbol "fstat"   'a6le))

;; ======================================================================
;; arm64 macOS -- no legacy variant exists, so the bare symbol is already 64-bit
;; ======================================================================
;; Darwin on arm64 never shipped a 32-bit-inode stat/readdir, so its bare symbol
;; IS the 64-bit-inode one and there is no `…$INODE64` entry point to name. The
;; gate must never append the suffix here.

(test-equal "readdir/arm64osx keeps the bare symbol"
  "readdir" (inode64-symbol "readdir" 'arm64osx))
(test-equal "stat/tarm64osx keeps the bare symbol"
  "stat"    (inode64-symbol "stat"    'tarm64osx))

;; ======================================================================
;; Strictly gated -- neither 32-bit Intel macOS nor FreeBSD gets the variant
;; ======================================================================
;; The suffix is scoped to a6osx/ta6osx alone. 32-bit Intel macOS (i3osx) is out
;; of scope, and FreeBSD never had the inode-width split at all; both keep the
;; bare symbol. These guard the strict gate behaviourally, so a future widening of
;; the case that leaked the variant onto another target would fail here.

(test-equal "stat/i3osx keeps the bare symbol (32-bit Intel macOS is out of scope)"
  "stat" (inode64-symbol "stat" 'i3osx))
(test-equal "stat/a6fb keeps the bare symbol (FreeBSD never had the split)"
  "stat" (inode64-symbol "stat" 'a6fb))

(test-end)
