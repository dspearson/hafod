;;; (hafod internal platform) -- The single owner of (machine-type).
;;; Decodes (machine-type) once into os-family / cpu-arch symbols and the
;;; resolved platform artefacts -- the thread-safe errno accessor name -- so no
;;; other library re-derives a platform decision.
;;; An unrecognised machine-type raises a clear, named error rather than
;;; silently assuming Linux.
;;; Imports ONLY (chezscheme): it reads (machine-type) and must stay free of any
;;; dependency on errno.ss (which imports this hub), so it cannot use
;;; raise-posix-error -- the built-in assertion-violation reports the error.
;;; Copyright (c) 2026, hafod contributors.

(library (hafod internal platform)
  (export
    ;; Resolved platform identity (interned symbols for the build host).
    os-family cpu-arch
    ;; Pure parsers: machine-type symbol -> family / architecture symbol.
    machine-type->os-family machine-type->cpu-arch
    ;; Resolved artefact relocated out of errno.ss.
    errno-accessor-name)
  (import (chezscheme))

  ;; ======================================================================
  ;; Pure machine-type parsers
  ;; ======================================================================
  ;; Procedures of a machine-type symbol (NOT a closed-over read of the live
  ;; (machine-type)), so the decode for every target -- including macOS and
  ;; FreeBSD -- is exercised on the Linux host. The enumerations mirror the
  ;; in-repo machine-type cases (errno.ss errno accessor + posix-misc.ss uname);
  ;; threaded ("t"-prefixed) and non-threaded forms decode identically.

  (define (machine-type->os-family mt)
    (case mt
      [(a6le ta6le arm64le tarm64le i3le ti3le) 'linux]
      [(a6osx ta6osx arm64osx tarm64osx i3osx ti3osx) 'macos]
      [(a6fb ta6fb arm64fb tarm64fb i3fb ti3fb) 'freebsd]
      [else (assertion-violation 'machine-type->os-family
              "unrecognised machine-type; refusing to assume Linux" mt)]))

  (define (machine-type->cpu-arch mt)
    (case mt
      [(a6le ta6le a6osx ta6osx a6fb ta6fb) 'x86-64]
      [(arm64le tarm64le arm64osx tarm64osx arm64fb tarm64fb) 'aarch64]
      [(i3le ti3le i3osx ti3osx i3fb ti3fb) 'i386]
      [else (assertion-violation 'machine-type->cpu-arch
              "unrecognised machine-type; cannot determine CPU arch" mt)]))

  ;; ======================================================================
  ;; Resolved identity -- the single live (machine-type) read
  ;; ======================================================================
  ;; This is the one and only place the live (machine-type) is consulted; every
  ;; consumer imports the resolved results below.

  (define os-family (machine-type->os-family (machine-type)))
  (define cpu-arch  (machine-type->cpu-arch  (machine-type)))

  ;; ======================================================================
  ;; Resolved platform artefacts
  ;; ======================================================================

  ;; Thread-safe errno access: the accessor name differs across platforms --
  ;; glibc/musl use __errno_location, macOS/FreeBSD use __error.
  (define errno-accessor-name
    (case os-family [(macos freebsd) "__error"] [else "__errno_location"]))

  ) ; end library
