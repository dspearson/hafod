#!/bin/sh
# test-history-eof.sh -- The exit hooks must run when a run ends, whichever way it
# ends: the input reaching EOF, a script reaching its last form, a command finishing.
#
# The hooks run from exactly one place -- hafod's exit -- and the ordinary ways out
# never reached it.  The REPL's read returns the eof object, interactive-repl returns,
# the launcher's dispatch returns, and the program falls off the end, running neither
# the exit hooks nor Chez's exit-handler; a script that reaches its last form and a -c
# expression that finishes fall off that same end by that same route.  So the hooks ran
# when a run typed (exit), and did not run when a user pressed Ctrl-D, when a script
# ended, or when a command finished.
#
# add-exit-hook! is public API, so the hook left unrun is a user's as much as hafod's
# own: the one their script registered to close a database, drop a lock file or flush a
# report on the way out.  hafod's history is the case that shows it up.  The hook the
# history registers when it opens closes the SQLite connection -- and an open connection
# left for the kernel to reclaim at teardown is not a closed one, which hands SQLite no
# chance to finish with the file.
#
# With the database in WAL that is user-visible.  A WAL database keeps a -wal and a -shm
# beside it while a connection is open, and the -wal is where the lines just typed
# actually sit until something checkpoints them back into the database.  A process that
# leaves without closing leaves them where they lie -- so every Ctrl-D exit stranded
# ~/.hafod_history.db-wal in the user's home directory, holding the commands just typed
# (strings(1) reads them straight out), for good; and every script that opened a history
# stranded one beside its own.
#
# Closing the connection is necessary but NOT sufficient, which is the second half of
# this: SQLite unlinks the -wal only when the last connection to close can take the
# database exclusively, and Darwin's SQLite keeps the sidecars even then (its own
# sqlite3(1) does the same).  So history-close! switches the journal back to DELETE
# before it closes -- checkpointing the log INTO the database and unlinking it -- and the
# -wal assertions below therefore hold on EVERY platform, with no exception.  The -shm
# that Darwin strands regardless is a hash index into the log and not the log; what it
# must not contain is asserted, not waived.  See assert_no_shm.
#
# The proof drives the real bin/hafod on each of its ways out -- a session on a PIPE, a
# script with -s, a command with -c, a script named on its own -- and asserts on what
# each leaves behind: the hook the run itself registered, counted, and the files beside
# the database.  A pipe that reaches its end delivers exactly the EOF a terminal
# delivers on Ctrl-D -- the same eof object, from the same read, into the same branch --
# so this needs no terminal and runs the same way on every platform and CI leg.  Each
# run opens a history of its own under a scratch HOME, so nothing here can read or write
# the history a developer running it actually has.
#
# The sidecars are looked for BEFORE anything reads the database back: opening a
# database with a stranded -wal is what RECOVERS it, checkpointing the log and
# removing the very files this is here to find.
#
# Copyright (c) 2026 Dominic Pearson.

set -e

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
HAFOD="$ROOT/bin/hafod"
PASS=0
FAIL=0
TOTAL=0

TMP="$(mktemp -d "${TMPDIR:-/tmp}/hafod-history-eof-XXXXXX")"
trap 'rm -rf "$TMP"' EXIT INT TERM HUP

pass() {
    PASS=$((PASS + 1))
    TOTAL=$((TOTAL + 1))
    printf "  PASS: %s\n" "$1"
}

fail() {
    FAIL=$((FAIL + 1))
    TOTAL=$((TOTAL + 1))
    printf "  FAIL: %s\n" "$1"
    if [ -n "${2-}" ]; then
        printf "    expected: %s\n" "$2"
        printf "    actual:   %s\n" "$3"
    fi
}

assert_equal() {
    if [ "$2" = "$3" ]; then pass "$1"; else fail "$1" "$2" "$3"; fi
}

assert_file() {
    if [ -f "$2" ]; then pass "$1"; else fail "$1" "present" "absent"; fi
}

assert_no_file() {
    if [ ! -f "$2" ]; then pass "$1"; else fail "$1" "absent" "present"; fi
}

# The wal-index (-shm) beside a WAL database.  A glibc SQLite unlinks it along with the
# -wal when the last connection closes cleanly, and none should survive.  Darwin's SQLite
# does not: it strands the -shm even on a clean close in DELETE journal mode -- its own
# /usr/bin/sqlite3 leaves one behind exactly the same way -- and no supported call gets
# rid of it.  Unlinking a live wal-index by hand is not an option: it is how you corrupt a
# database that another session still has open, which is the whole reason SQLite only ever
# removes it while holding the database exclusively.
#
# So assert the property that actually protects the user, rather than a file count.  The
# -shm is a hash index INTO the log, not the log; what the user typed lives in the -wal,
# and history-close! now checkpoints that away on EVERY platform (that is the fix, and the
# -wal assertions above hold everywhere without exception).  Where the platform strands the
# -shm regardless, prove it holds NONE of what was typed.  That is the thing the sidecar
# assertions were written to defend -- so it is asserted here, not waived.
#
#   assert_no_shm NAME SHM-PATH TYPED-LINE
assert_no_shm() {
    if [ ! -f "$2" ]; then
        pass "$1"
    elif [ "$(uname -s)" != "Darwin" ]; then
        fail "$1" "absent" "present"
    elif LC_ALL=C grep -qa -- "$3" "$2"; then
        fail "$1" "the wal-index Darwin strands holds none of the typed line" \
             "it holds '$3'"
    else
        pass "$1 (Darwin strands the wal-index; it holds none of the typed line)"
    fi
}

section() { printf "\n=== %s ===\n" "$1"; }

# Run a hafod session on a pipe and report its exit status.  Its HOME is the
# scratch directory and its config file is skipped, so the session reads nothing of
# the developer's and writes nothing outside $TMP.  Stdin reaching its end is what
# ends the session, unless the input itself said otherwise.
#
#   session OUT-FILE INPUT
run_session() {
    out="$1"
    input="$2"
    status=0
    printf '%s' "$input" \
        | env HOME="$TMP" XDG_CONFIG_HOME="$TMP/config" "$HAFOD" --no-config \
            >"$out" 2>&1 || status=$?
    return "$status"
}

# Run a hafod BOOT -- a script (-s), a command (-c), a bare filename -- under the same
# scratch HOME, and report its exit status.  Its stdin is /dev/null: none of these paths
# reads input, and one that did would otherwise hang the suite waiting for a terminal
# that is not there.
#
#   run_boot OUT-FILE ARG...
run_boot() {
    out="$1"
    shift
    status=0
    env HOME="$TMP" XDG_CONFIG_HOME="$TMP/config" "$HAFOD" --no-config "$@" \
        </dev/null >"$out" 2>&1 || status=$?
    return "$status"
}

# The line the session types.  It goes into the history database, so it is also
# what a stranded -wal would be holding.
LINE='ls -la /etc'

# The marker an exit hook prints when it runs.  Counting it is how "the hooks ran"
# and "the hooks ran ONCE" are told apart -- a fix that both composed an exit
# handler and exited through the hooks would run them twice, and print it twice.
MARKER='the-exit-hooks-ran'

# The Scheme that registers it, through the PUBLIC add-exit-hook! -- the very call a
# user's script makes.  One definition, used verbatim by every way out below, so each is
# asked the same question in the same words: did the hook this run registered run?
HOOK="$(printf '(add-exit-hook! (lambda () (display "%s\\n" (console-error-port))))' \
    "$MARKER")"

# How many times the hooks ran: the hook prints one marker line per run, so the count of
# marker lines IS the count of runs.  Zero is the defect seen directly -- they never ran;
# two is the double-run, which re-runs every hook a user registered and closes twice what
# they close.  grep -c reports 0 and exits 1 when it finds nothing, which is not a failure
# here but an answer, so take the count and let the status go.
hook_runs() {
    grep -c "$MARKER" "$1" || true
}

# ======================================================================
section "the input ends (the pipe reaches EOF -- the Ctrl-D path)"
# ======================================================================

EOF_DB="$TMP/ended-history.db"
status=0
run_session "$TMP/eof.out" "$(printf '(define h (open-history "%s"))\n(history-add! h "%s")\n' "$EOF_DB" "$LINE")" || status=$?

# Control: the session ran to a clean end.  A crash on the way out would leave the
# sidecars behind too, and would say nothing at all about closing the database.
assert_equal "the session that reached the end of its input exits cleanly" "0" "$status"

# Control: it really did open a history database.  Without this, a session that had
# quietly failed to open one would leave no sidecars either -- and pass everything
# below while proving nothing.
assert_file "the session opened a history database" "$EOF_DB"

# The observation.  Both sidecars are gone, so the last connection was closed:
# SQLite checkpointed the log back into the database and removed them.  Before the
# fix the process left without closing and both files stayed -- in $HOME, holding
# the line just typed.
assert_no_file "no write-ahead log is left beside the database when the input ends" \
    "$EOF_DB-wal"
assert_no_shm "no shared-memory file is left beside the database when the input ends" \
    "$EOF_DB-shm" "$LINE"

# Control, and only now that the sidecars have been looked for: the line reached the
# database file itself.  So the session did the work whose leavings were just
# checked, and the close carried the log home rather than abandoning it.
rows=$(env HOME="$TMP" "$HAFOD" --no-config -c "(let* ([db (sqlite3-open \"$EOF_DB\")] [stmt (sqlite3-prepare db \"SELECT COUNT(*) FROM history WHERE input = '$LINE'\")]) (sqlite3-step stmt) (display (sqlite3-column-int64 stmt 0)) (sqlite3-finalize stmt) (sqlite3-close db))" 2>/dev/null | tail -1)
assert_equal "the line the session typed is in the database it left behind" "1" "$rows"

# ======================================================================
section "an explicit exit (the path that already closed cleanly)"
# ======================================================================

# The control path.  It ran the hooks before this fix and must still run them --
# exactly once -- after it.
EXIT_DB="$TMP/exited-history.db"
status=0
run_session "$TMP/exit.out" "$(printf '(define h (open-history "%s"))\n(history-add! h "%s")\n(exit)\n' "$EXIT_DB" "$LINE")" || status=$?

assert_equal "the session that typed exit leaves cleanly" "0" "$status"
assert_file "the session opened a history database" "$EXIT_DB"
assert_no_file "no write-ahead log is left beside the database after an explicit exit" \
    "$EXIT_DB-wal"
assert_no_shm "no shared-memory file is left beside the database after an explicit exit" \
    "$EXIT_DB-shm" "$LINE"

# ======================================================================
section "the exit hooks run, once, on both ways out of a session"
# ======================================================================

# Watch the hooks themselves rather than only their effect: a hook registered in the
# session prints a marker, and the marker is counted.  Zero means the hooks never
# ran (this is the defect, seen directly); two means they ran twice, which double-
# closes ports and re-runs every hook a user has registered.
run_session "$TMP/hook-eof.out" "$HOOK" || true
assert_equal "the exit hooks run exactly once when the input ends" \
    "1" "$(hook_runs "$TMP/hook-eof.out")"

run_session "$TMP/hook-exit.out" "$(printf '%s\n(exit)\n' "$HOOK")" || true
assert_equal "the exit hooks run exactly once on an explicit exit" \
    "1" "$(hook_runs "$TMP/hook-exit.out")"

# ======================================================================
section "a script reaches its end (-s)"
# ======================================================================

# A session is not the only thing that ends.  A script's last form is evaluated,
# load-script-file returns, the dispatch returns, and the program falls off exactly the
# end the REPL's EOF fell off -- so a script's exit hooks went unrun for the same reason
# a Ctrl-D's did.  The hook here is the user's own, registered through the public
# add-exit-hook! from inside the script, which is the whole of the case: whatever a
# script arranged to do on its way out, hafod simply did not do.
printf '%s\n' "$HOOK" > "$TMP/hook.ss"
status=0
run_boot "$TMP/script-hook.out" -s "$TMP/hook.ss" || status=$?
assert_equal "a script that reaches its end exits cleanly" "0" "$status"
assert_equal "the exit hooks run exactly once when a script reaches its end" \
    "1" "$(hook_runs "$TMP/script-hook.out")"

# And the consequence, on hafod's own hook.  The script opens a history; the hook the
# history registers closes it.  The database is opened under the scratch HOME and named
# explicitly, so this is the script's own file and no developer's.
SCRIPT_DB="$TMP/script-history.db"
printf '(define h (open-history "%s"))\n(history-add! h "%s")\n' "$SCRIPT_DB" "$LINE" \
    > "$TMP/history.ss"
status=0
run_boot "$TMP/script-history.out" -s "$TMP/history.ss" || status=$?

assert_equal "the script that opened a history exits cleanly" "0" "$status"

# Control, as above: it really did open one.  A script that had quietly failed to would
# leave no sidecars either, and pass the two assertions below while proving nothing.
assert_file "the script opened a history database" "$SCRIPT_DB"

# The observation.  Both sidecars are gone, so the connection the script opened was
# closed and the log checkpointed home.  Before this the script simply ended, nothing
# closed it, and both files were left beside the database holding the line just added.
assert_no_file "no write-ahead log is left beside the database a script opened" \
    "$SCRIPT_DB-wal"
assert_no_shm "no shared-memory file is left beside the database a script opened" \
    "$SCRIPT_DB-shm" "$LINE"

# ======================================================================
section "a command reaches its end (-c)"
# ======================================================================

# The same end, one branch over: -c evaluates its expression and returns.
status=0
run_boot "$TMP/command-hook.out" -c "$HOOK" || status=$?
assert_equal "a command that reaches its end exits cleanly" "0" "$status"
assert_equal "the exit hooks run exactly once when a command reaches its end" \
    "1" "$(hook_runs "$TMP/command-hook.out")"

COMMAND_DB="$TMP/command-history.db"
status=0
run_boot "$TMP/command-history.out" -c \
    "$(printf '(define h (open-history "%s")) (history-add! h "%s")' "$COMMAND_DB" "$LINE")" \
    || status=$?

assert_equal "the command that opened a history exits cleanly" "0" "$status"
assert_file "the command opened a history database" "$COMMAND_DB"
assert_no_file "no write-ahead log is left beside the database a command opened" \
    "$COMMAND_DB-wal"
assert_no_shm "no shared-memory file is left beside the database a command opened" \
    "$COMMAND_DB-shm" "$LINE"

# ======================================================================
section "a script named on its own (the #!/usr/bin/env hafod path)"
# ======================================================================

# The same script, named without -s.  A shebang boots through this branch, so it is the
# one most scripts a user writes actually take -- and it fell off the same end.
status=0
run_boot "$TMP/bare-hook.out" "$TMP/hook.ss" || status=$?
assert_equal "a bare script that reaches its end exits cleanly" "0" "$status"
assert_equal "the exit hooks run exactly once when a bare script reaches its end" \
    "1" "$(hook_runs "$TMP/bare-hook.out")"

# ======================================================================
section "a script or command that exits for itself"
# ======================================================================

# These two already ran the hooks: an explicit (exit N) goes straight out through hafod's
# exit, from wherever it is written.  They are here to pin what must not be disturbed by
# making the ends above leave the same way.
#
# The status is the SCRIPT's, not the launcher's -- a script that fails must still fail,
# so a launcher that exited 0 of its own accord after the script returned would report
# success for a script that asked for 3.  And the hooks must run ONCE, not once for the
# script's own exit and again on the way out of the launcher: a run that closes a
# database, drops a lock file or posts a report twice is its own defect.
printf '%s\n(exit 3)\n' "$HOOK" > "$TMP/hook-exit3.ss"
status=0
run_boot "$TMP/script-exit3.out" -s "$TMP/hook-exit3.ss" || status=$?
assert_equal "a script that exits 3 still exits 3" "3" "$status"
assert_equal "the exit hooks run exactly once when a script exits for itself" \
    "1" "$(hook_runs "$TMP/script-exit3.out")"

status=0
run_boot "$TMP/command-exit3.out" -c "$HOOK (exit 3)" || status=$?
assert_equal "a command that exits 3 still exits 3" "3" "$status"
assert_equal "the exit hooks run exactly once when a command exits for itself" \
    "1" "$(hook_runs "$TMP/command-exit3.out")"

# ======================================================================
# Summary
# ======================================================================

printf "\n=== Summary ===\n"
printf "%d passed, %d failed (out of %d)\n" "$PASS" "$FAIL" "$TOTAL"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
