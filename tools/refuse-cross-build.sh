#!/bin/sh
# refuse-cross-build.sh -- refuse a Rosetta-translated or cross/emulated build
# before any architecture-specific constant is generated, so wrong-arch struct
# offsets can never be baked into the artefacts.
#
# Two independent guards:
#   1. macOS Rosetta 2 -- sysctl.proc_translated == 1 means the process is being
#      translated; only an explicit 1 refuses. The key is absent on Intel Macs
#      (and `sysctl` itself is absent off macOS), so an empty/missing value must
#      proceed, never refuse.
#   2. Universal arch consistency -- the host machine must match the target the
#      toolchain emits. Architecture spellings are normalised first
#      (amd64 == x86_64, arm64 == aarch64, i?86 == i386) so a native FreeBSD
#      (amd64) or aarch64 build is never wrongly refused.
#
# Single-host testable: UNAME_S, UNAME_M and CC pick the platform, and the
# test-only PROC_TRANSLATED and CC_DUMPMACHINE overrides drive every branch with
# no real cross toolchain (the build leaves them unset, so the real probes run).
#
# Usage:
#   tools/refuse-cross-build.sh    Exit 0 to proceed, non-zero (with a message
#                                  on stderr) to refuse.
set -eu

: "${UNAME_S:=$(uname -s)}"
: "${UNAME_M:=$(uname -m)}"
: "${CC:=cc}"
dm="${CC_DUMPMACHINE:-$("$CC" -dumpmachine 2>/dev/null || true)}"

# Reduce an architecture name to one canonical spelling so the host and the
# toolchain target are compared like for like.
norm() { case "$1" in amd64) echo x86_64;; arm64) echo aarch64;; i?86) echo i386;; *) echo "$1";; esac; }

# (1) macOS Rosetta 2: only a literal proc_translated=1 means we are translated.
if [ "$UNAME_S" = "Darwin" ]; then
  pt="${PROC_TRANSLATED:-$(sysctl -in sysctl.proc_translated 2>/dev/null || true)}"
  if [ "$pt" = "1" ]; then
    echo "refuse: this build is running under Rosetta 2 (sysctl.proc_translated=1)." >&2
    echo "refuse: native struct offsets would be baked for the wrong architecture." >&2
    echo "refuse: re-run in a native arm64 shell (e.g. arch -arm64 \$SHELL)." >&2
    exit 1
  fi
fi

# (2) Universal: the host architecture must match the toolchain's target arch.
host="$(norm "$UNAME_M")"
tgt="$(norm "${dm%%-*}")"
if [ -n "$tgt" ] && [ "$host" != "$tgt" ]; then
  echo "refuse: host architecture '$UNAME_M' != toolchain target '$dm' (arch '$tgt')." >&2
  echo "refuse: a cross/emulated toolchain would bake wrong-arch offsets; aborting." >&2
  exit 1
fi
exit 0
