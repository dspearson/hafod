;;; (test test-platform) -- Decode assertions for the (hafod internal platform) hub.
;;; The hub's machine-type parsers are pure procedures of a machine-type symbol,
;;; so this suite exercises the macOS/FreeBSD/Linux decode matrix and the
;;; unknown-machine-type error entirely on the Linux CI host -- no real macOS or
;;; FreeBSD machine is needed.
;;; Copyright (c) 2026, hafod contributors.

(library-directories '(("src" . "src") ("." . ".")))
(import (test runner)
        (hafod internal platform))

(test-begin "platform")

;; ======================================================================
;; machine-type->os-family -- pure decode of the design-target vocabulary
;; ======================================================================
;; Every Chez design-target machine-type maps to exactly one OS family.
;; Threaded (leading "t") and non-threaded forms decode identically.

(test-equal "ta6le decodes to linux"      'linux   (machine-type->os-family 'ta6le))
(test-equal "a6le decodes to linux"       'linux   (machine-type->os-family 'a6le))
(test-equal "tarm64le decodes to linux"   'linux   (machine-type->os-family 'tarm64le))
(test-equal "ti3le decodes to linux"      'linux   (machine-type->os-family 'ti3le))

(test-equal "tarm64osx decodes to macos"  'macos   (machine-type->os-family 'tarm64osx))
(test-equal "a6osx decodes to macos"      'macos   (machine-type->os-family 'a6osx))
(test-equal "ta6osx decodes to macos"     'macos   (machine-type->os-family 'ta6osx))
(test-equal "ti3osx decodes to macos"     'macos   (machine-type->os-family 'ti3osx))

(test-equal "ta6fb decodes to freebsd"    'freebsd (machine-type->os-family 'ta6fb))
(test-equal "a6fb decodes to freebsd"     'freebsd (machine-type->os-family 'a6fb))
(test-equal "tarm64fb decodes to freebsd" 'freebsd (machine-type->os-family 'tarm64fb))
(test-equal "ti3fb decodes to freebsd"    'freebsd (machine-type->os-family 'ti3fb))

;; ======================================================================
;; machine-type->cpu-arch -- pure decode of the architecture token
;; ======================================================================
;; a6 -> x86-64, arm64 -> aarch64, i3 -> i386, across all OS suffixes.

(test-equal "ta6le decodes to x86-64"     'x86-64  (machine-type->cpu-arch 'ta6le))
(test-equal "ta6osx decodes to x86-64"    'x86-64  (machine-type->cpu-arch 'ta6osx))
(test-equal "ta6fb decodes to x86-64"     'x86-64  (machine-type->cpu-arch 'ta6fb))

(test-equal "tarm64osx decodes to aarch64" 'aarch64 (machine-type->cpu-arch 'tarm64osx))
(test-equal "arm64le decodes to aarch64"   'aarch64 (machine-type->cpu-arch 'arm64le))
(test-equal "tarm64fb decodes to aarch64"  'aarch64 (machine-type->cpu-arch 'tarm64fb))

(test-equal "ti3le decodes to i386"       'i386    (machine-type->cpu-arch 'ti3le))
(test-equal "i3osx decodes to i386"       'i386    (machine-type->cpu-arch 'i3osx))

;; ======================================================================
;; Unknown machine-type -- a loud, named error, never a silent Linux default
;; ======================================================================
;; A machine-type the hub does not recognise must raise rather than silently
;; assume Linux: mis-classifying a foreign target as Linux would run the wrong
;; FFI code path. Both parsers reject the unknown symbol.

(test-error "machine-type->os-family rejects an unknown machine-type"
  (machine-type->os-family 'zz9plural-z-alpha))
(test-error "machine-type->cpu-arch rejects an unknown machine-type"
  (machine-type->cpu-arch 'zz9plural-z-alpha))

;; ======================================================================
;; Resolved live values -- the single (machine-type) read produced symbols
;; ======================================================================
;; The parsers are procedures and the eagerly-resolved os-family / cpu-arch
;; are interned symbols for the build host.

(test-assert "machine-type->os-family is a procedure" (procedure? machine-type->os-family))
(test-assert "machine-type->cpu-arch is a procedure"  (procedure? machine-type->cpu-arch))
(test-assert "os-family is a resolved symbol"         (symbol? os-family))
(test-assert "cpu-arch is a resolved symbol"          (symbol? cpu-arch))

(test-end)
