#!/bin/sh
# test-spawn-fa-opaque.sh -- Static gate: the posix_spawn_file_actions_t blob
# stays opaque. The byte blob is owned by the libc spawn API; Scheme only
# allocates it and passes a uptr to init/add*/destroy -- it must never read or
# write the struct at any offset. This gate fails if any foreign-ref/foreign-set!
# names the file-actions blob, so a libc-private layout change cannot corrupt it.
# (The lone foreign-ref in that block reads the int pid-buf, not the blob.)
#
# Copyright (c) 2026 Dominic Pearson.

set -e

SRC="$(dirname "$0")/../src/hafod/internal/posix-core.ss"

if grep -nE 'foreign-(ref|set!)' "$SRC" \
     | grep -iE 'fa\b|fileact|spawn-fa|FILEACT'; then
  echo "FAIL: spawn-file-actions struct is offset-accessed"; exit 1
fi
echo "PASS: spawn-fa remains opaque"
