#!/bin/sh
# test-ffi-no-helper.sh -- Prove the variadic syscalls work with no helper
# library present. open/fcntl/ioctl call libc directly through Chez's native
# variadic convention, so the suite must pass with the helper shared object
# moved off the search path. This harness stashes the helper object, runs the
# full suite with it absent, and restores it on EXIT (success, failure, or
# signal) so a working tree is never left without it.
#
# It runs the suite's own `make test` internally, so it is invoked ONLY as a
# standalone target -- never folded into the `test:` aggregate, which would
# recurse a fresh `make test` into itself indefinitely.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

cd "$(dirname "$0")/.."

# The helper object is built as .so on Linux and .dylib on macOS; stash
# whichever is present so this harness proves the native path on either.
for ext in so dylib; do
  SO="src/hafod-ffi-helpers.$ext"
  [ -f "$SO" ] && mv "$SO" "$SO.stash" || true
done

# Restore every stashed object on any exit path.
trap '
  for ext in so dylib; do
    SO="src/hafod-ffi-helpers.$ext"
    [ -f "$SO.stash" ] && mv "$SO.stash" "$SO" || true
  done
' EXIT

nix develop -c make test          # must pass with NO helper object present
