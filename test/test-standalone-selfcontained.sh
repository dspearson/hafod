#!/bin/sh
# test-standalone-selfcontained.sh -- Prove the standalone binary carries its
# libraries and opens none.
#
# bin/hafod-standalone is not the launcher with the libraries bolted on: it is a
# single image. tools/build-standalone.ss bakes every hafod library into an
# embedded vfasl boot file, which tools/hafod-standalone.c registers from memory
# before a line of Scheme runs, so the libraries are already in the heap at main
# and nothing is ever loaded from disk. Nothing else in the tree asserts that,
# and the property is invisible from a normal working directory -- where a real
# src/ sits under the binary's feet and would quietly answer any import it got
# wrong.
#
# So run it where it can cheat from nowhere: an EMPTY directory. The binary
# never sets library-directories, so it keeps Chez's default of ".", and "." is
# then a directory containing nothing at all. Every library the binary resolves
# from there came out of its own image. If it needed a file it would have to say
# so, and every entry point -- -c, -s, -e, -l and the piped REPL -- would fail.
#
# This is what makes the proof non-vacuous. Two plausible "improvements" fail it
# outright:
#
#   * Compile the program with compile-whole-program and drop the boot image
#     (the merge inlines a library's run-time code but not its compile-time
#     visit information, so a dynamic import still wants the file): --version
#     survives, and -c / -s / -e / -l / the REPL all die with
#     "Exception: library (hafod) not found".
#   * Build the boot image from the COMPILED objects rather than the sources, as
#     a whole-program merge of this program requires: only the libraries with a
#     .so exist, and the count assertion and (import (hafod srfi-9)) below both
#     fail -- that variant silently loses every SRFI.
#
# Heavy by design: it builds the standalone binary, so -- like test-ffi-no-helper,
# test-hafod-so-fresh and test-install-launch -- it is a STANDALONE target, never
# folded into the `test:` aggregate.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

# The binary under proof. Overridable so a candidate image can be driven through
# the same assertions without being installed over the shipped one.
BIN="${HAFOD_STANDALONE:-$ROOT/bin/hafod-standalone}"

# Chez reads CHEZSCHEMELIBDIRS and CHEZSCHEMELIBEXTS from the environment, and
# the standalone's C launcher never calls library-directories -- so a value left
# in the caller's environment would point the binary at a real library tree and
# quietly turn every assertion below into a tautology. Clear them: the empty
# directory must be the only place this binary could possibly look.
unset CHEZSCHEMELIBDIRS CHEZSCHEMELIBEXTS

PASS=0
FAIL=0
TOTAL=0

pass() {
    PASS=$((PASS + 1))
    TOTAL=$((TOTAL + 1))
    printf "  PASS: %s\n" "$1"
}

fail() {
    FAIL=$((FAIL + 1))
    TOTAL=$((TOTAL + 1))
    printf "  FAIL: %s\n" "$1"
    if [ -n "$2" ]; then
        printf "    expected: %s\n" "$2"
        printf "    actual:   %s\n" "$3"
    fi
}

skip() {
    # Not an assertion: neither passed nor failed, and not counted. Used only
    # where the probe itself is platform-specific.
    printf "  SKIP: %s\n" "$1"
}

assert_eq() {
    # $1=description $2=expected $3=actual
    if [ "$2" = "$3" ]; then
        pass "$1"
    else
        fail "$1" "$2" "$3"
    fi
}

section() {
    printf "\n=== %s ===\n" "$1"
}

if [ ! -x "$BIN" ]; then
    printf "test-standalone-selfcontained: %s is missing (run 'make standalone')\n" "$BIN" >&2
    exit 1
fi

# EMPTY is the binary's working directory: it must stay empty, so the scripts the
# -s / -e / -l cases need live in WORK and are named by absolute path. FAKEHOME
# keeps the REPL's history database and any user config out of the run (and the
# run out of the user's). All three go on every exit path.
EMPTY="$(mktemp -d "${TMPDIR:-/tmp}/hafod-selfcontained-empty-XXXXXX")"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/hafod-selfcontained-work-XXXXXX")"
FAKEHOME="$(mktemp -d "${TMPDIR:-/tmp}/hafod-selfcontained-home-XXXXXX")"
trap 'rm -rf "$EMPTY" "$WORK" "$FAKEHOME"' EXIT INT TERM HUP

printf '(display "script ok")(newline)\n'                        > "$WORK/script.ss"
printf '(define (main args) (display "entry ok")(newline))\n'    > "$WORK/entry.ss"
printf '(define preloaded "preload ok")\n'                       > "$WORK/preload.ss"

# Run the binary from the empty directory, leaving its combined output in $out
# and its status in $code. The `set +e` is load-bearing: an image that cannot
# resolve (hafod) exits 255, and under `set -e` an unguarded command
# substitution would take the whole run down with it -- abandoning the file
# mid-assertion with no diagnostic printed at all. A non-zero exit is a result
# to assert on, not a reason to stop reporting.
run_bin() {
    set +e
    out=$(cd "$EMPTY" && HOME="$FAKEHOME" "$BIN" "$@" 2>&1)
    code=$?
    set -e
}

# Assert the binary printed the marker AND exited 0. One assertion per entry
# point: an image that cannot resolve (hafod) raises an exception and exits
# non-zero, so either half of the check catches it.
assert_mode() {
    desc="$1"
    marker="$2"
    shift 2
    run_bin "$@"
    case "$out" in
        *"$marker"*)
            if [ "$code" -eq 0 ]; then
                pass "$desc"
            else
                fail "$desc" "exit 0" "exit $code: $out"
            fi ;;
        *)
            fail "$desc" "output containing '$marker', exit 0" "exit $code: $out" ;;
    esac
}

section "The image carries its libraries (empty working directory, no src/)"

# The binary looks in "." and nowhere else -- and "." is empty. Assert the search
# path itself, so a future launcher that quietly pointed the binary at a real
# library tree could not pass the rest of this file by cheating off the disk.
run_bin -c '(write (library-directories))'
assert_eq "the image searches only the (empty) working directory" '(("." . "."))' "$out"

# Count the library sources the way tools/build-standalone.ss discovers them:
# every .ss under src/hafod, plus the src/hafod.ss umbrella. Derived, never
# hardcoded -- a new library must appear in the image, and this keeps saying so
# without anyone remembering to bump a number.
SRC_LIBS=$(( $(find src/hafod -name '*.ss' | wc -l) + 1 ))
run_bin -c \
    '(display (length (filter (lambda (l) (and (pair? l) (eq? (car l) (quote hafod)))) (library-list))))'
assert_eq "every library source in the tree is in the image's heap" "$SRC_LIBS" "$out"

# The SRFIs are the ones a merge-built image loses: only the libraries that have
# a compiled object can go into a boot image built from objects, and the SRFIs
# have none. They are reachable here because the image was built from SOURCE.
assert_mode "the SRFIs import with no filesystem to import them from" \
    "srfi ok" -c '(import (hafod srfi-9))(display "srfi ok")'

assert_mode "the (hafod) umbrella imports with no filesystem to import it from" \
    "umbrella ok" -c '(import (hafod))(display "umbrella ok")'

section "Every entry point runs from an empty directory"

assert_mode "--version runs" "hafod" --version
assert_mode "-c evaluates an expression" "expr ok" -c '(display "expr ok")'
assert_mode "-s runs a script" "script ok" -s "$WORK/script.ss"
assert_mode "-e calls the entry point of a script" "entry ok" -e main -s "$WORK/entry.ss"
assert_mode "-l preloads a file" "preload ok" -l "$WORK/preload.ss" -c '(display preloaded)'

# The REPL: stdin on a pipe is the eof a terminal gives on Ctrl-D, so no terminal
# is needed. --no-config keeps a user's init file out of the result.
set +e
out=$(cd "$EMPTY" && printf '(display (+ 1 2))\n' | HOME="$FAKEHOME" "$BIN" --no-config 2>&1)
code=$?
set -e
case "$out" in
    *3*) if [ "$code" -eq 0 ]; then
             pass "the REPL evaluates a line fed on stdin"
         else
             fail "the REPL evaluates a line fed on stdin" "exit 0" "exit $code: $out"
         fi ;;
    *) fail "the REPL evaluates a line fed on stdin" "output containing '3', exit 0" \
            "exit $code: $out" ;;
esac

section "It opens no library file at run time"

# LD_PRELOAD is the Linux mechanism; macOS blocks DYLD_INSERT_LIBRARIES for many
# binaries under SIP, so this ONE probe is gated. Everything above is portable
# and is the substantive proof -- a library that is not on disk cannot have been
# loaded from disk, whatever the platform. This section corroborates it directly.
if [ "$(uname -s)" != "Linux" ]; then
    skip "run-time file opens are only probed on Linux (LD_PRELOAD)"
else
    cat > "$WORK/trace-open.c" <<'PROBE'
/* Record every path libc is asked to open. Chez reaches the filesystem through
 * these four entry points; the log tells us whether the standalone touches a
 * library file at run time. */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int (*next_open)(const char *, int, ...);
static int (*next_openat)(int, const char *, int, ...);
static int logfd = -1;

static void probe_init(void) {
    if (!next_open)   next_open   = dlsym(RTLD_NEXT, "open");
    if (!next_openat) next_openat = dlsym(RTLD_NEXT, "openat");
    if (logfd == -1) {
        const char *path = getenv("HAFOD_OPEN_LOG");
        /* Open the log through the REAL open, never our own symbol: no recursion. */
        logfd = (path && next_open)
              ? next_open(path, O_WRONLY | O_CREAT | O_APPEND, 0600) : -2;
    }
}

static void note(const char *path) {
    if (logfd >= 0 && path) {
        char buf[4096];
        size_t n = strlen(path);
        if (n > sizeof(buf) - 2) n = sizeof(buf) - 2;
        memcpy(buf, path, n);
        buf[n] = '\n';
        if (write(logfd, buf, n + 1) < 0) { /* the assertion will notice */ }
    }
}

#define MODE_FROM_VARARGS(flags)              \
    mode_t mode = 0;                          \
    va_list ap;                               \
    va_start(ap, flags);                      \
    if ((flags) & O_CREAT)                    \
        mode = (mode_t) va_arg(ap, int);      \
    va_end(ap)

int open(const char *path, int flags, ...) {
    MODE_FROM_VARARGS(flags);
    probe_init(); note(path);
    return next_open(path, flags, mode);
}

int open64(const char *path, int flags, ...) {
    MODE_FROM_VARARGS(flags);
    probe_init(); note(path);
    return next_open(path, flags, mode);
}

int openat(int dirfd, const char *path, int flags, ...) {
    MODE_FROM_VARARGS(flags);
    probe_init(); note(path);
    return next_openat(dirfd, path, flags, mode);
}

int openat64(int dirfd, const char *path, int flags, ...) {
    MODE_FROM_VARARGS(flags);
    probe_init(); note(path);
    return next_openat(dirfd, path, flags, mode);
}
PROBE

    CC="${CC:-cc}"
    # -ldl on a glibc older than 2.34; folded into libc after that, where naming
    # it is harmless but a hostile linker may still object. Try with, fall back.
    "$CC" -O2 -shared -fPIC -o "$WORK/trace-open.so" "$WORK/trace-open.c" -ldl 2>/dev/null \
        || "$CC" -O2 -shared -fPIC -o "$WORK/trace-open.so" "$WORK/trace-open.c"

    # A probe that observes nothing would let ANY binary pass the assertion that
    # follows -- silently, and for as long as nobody thinks to check. So make it
    # prove it can see, first: -s must open the script it is told to run, and the
    # probe must have that path in its log. Only a probe that catches the open it
    # is supposed to catch is allowed to certify the absence of the others.
    ctl_log="$WORK/control.log"
    (cd "$EMPTY" && HOME="$FAKEHOME" HAFOD_OPEN_LOG="$ctl_log" \
        LD_PRELOAD="$WORK/trace-open.so" "$BIN" -s "$WORK/script.ss" >/dev/null 2>&1) || true
    if [ -s "$ctl_log" ] && grep -q 'script\.ss' "$ctl_log"; then
        pass "the probe observes the standalone opening a file it is given"
    else
        fail "the probe observes the standalone opening a file it is given" \
             "script.ss in the open log" "$(cat "$ctl_log" 2>/dev/null)"
    fi

    # The real assertion: evaluating an expression reaches (hafod) through the
    # runtime (import (hafod)) in bin/hafod.sps -- the very path that sends the
    # merged LAUNCHER to disk for nineteen .so files. The standalone must open
    # none: no library object, no library source, no boot file.
    run_log="$WORK/run.log"
    (cd "$EMPTY" && HOME="$FAKEHOME" HAFOD_OPEN_LOG="$run_log" \
        LD_PRELOAD="$WORK/trace-open.so" "$BIN" -c '(void)' >/dev/null 2>&1) || true
    opened=$(grep -E '\.(so|ss|wpo|boot)$' "$run_log" 2>/dev/null | sort -u || true)
    if [ -z "$opened" ]; then
        pass "the standalone opens no library file to evaluate an expression"
    else
        fail "the standalone opens no library file to evaluate an expression" \
             "no .so/.ss/.wpo/.boot opened" "$(printf '%s' "$opened" | tr '\n' ' ')"
    fi
fi

printf "\n=== Summary ===\n"
printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
if [ "$FAIL" -gt 0 ]; then exit 1; fi
