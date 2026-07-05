#!/bin/sh
# platform-fingerprint.sh -- emit a one-line ABI fingerprint for the build host.
#
# Prints a hash of four facts: the operating system, the machine architecture,
# the compiler's own version banner, and the target triple the compiler emits.
# The result is recorded in .platform-stamp so that a compiler swap, a version
# bump, or an architecture/translation change is detected -- not merely a change
# of `uname`. A stamp keyed on `uname` alone would miss a gcc/clang swap or a
# Rosetta-translated host that still reports the same OS and machine.
#
# UNAME_S, UNAME_M and CC may be overridden in the environment to fingerprint
# another platform on a single host (the build leaves them unset, so the real
# probes are used). The hash is only ever compared on the same machine, so the
# particular hashing tool need not be identical from host to host.
#
# Usage:
#   tools/platform-fingerprint.sh        Print the one-line fingerprint.
set -eu

: "${UNAME_S:=$(uname -s)}"
: "${UNAME_M:=$(uname -m)}"
: "${CC:=cc}"

# The compiler's banner and target triple, each tolerant of a missing tool or
# field so an unusual host yields a stable line rather than aborting the build.
ccver=$("$CC" --version 2>/dev/null | head -n1 || true)
cctarget=$("$CC" -dumpmachine 2>/dev/null || true)

# Join the four facts newline-separated, then reduce them to one line with the
# first hasher that is present. macOS ships `shasum`, not GNU `sha256sum`, and a
# bare POSIX host has neither -- so probe a chain instead of assuming one tool.
payload=$(printf '%s\n%s\n%s\n%s\n' "$UNAME_S" "$UNAME_M" "$ccver" "$cctarget")
if   command -v sha256sum >/dev/null 2>&1; then h=$(printf '%s' "$payload" | sha256sum)
elif command -v shasum    >/dev/null 2>&1; then h=$(printf '%s' "$payload" | shasum -a 256)
elif command -v sha256    >/dev/null 2>&1; then h=$(printf '%s' "$payload" | sha256)   # FreeBSD
else                                           h=$(printf '%s' "$payload" | cksum)       # POSIX
fi

# Emit only the digest, dropping any trailing filename or size field the tool
# appends (cksum prints "<sum> <bytes>"; the shasum tools print "<sum>  -").
printf '%s\n' "${h%% *}"
