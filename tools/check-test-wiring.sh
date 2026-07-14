#!/bin/sh
# check-test-wiring.sh -- structural audit of the test-suite wiring.
#
# The build discovers its Scheme suites with ONE glob over test/*.ss, minus a
# short list of harness libraries held out by literal name. Nothing in make
# checks that those two sets between them account for every file in the
# directory, and nothing checks that a file is the KIND of thing the list it
# sits in says it is. Both gaps fail silently rather than loudly:
#
#   * a suite that matches neither list is simply never run, and nothing says
#     so -- which is how a set of compat suites sat in this tree unexecuted for
#     release after release, passing by never being asked; and
#
#   * Chez does not choke on a (library ...) form handed to --script. It defines
#     the library and exits 0, having printed nothing. So a harness library that
#     leaks into the suite list does not break the build -- it joins the run as
#     a suite that asserts nothing and always passes. A phantom green.
#
# This audit closes both, in five directions:
#
#   1. no unwired suite  -- every .ss under the test root is either wired into
#                           the run or declared a harness library;
#   2. no silenced suite -- every declared harness library really is a library,
#                           so a failing suite cannot be quieted by parking its
#                           name in the exclusion list;
#   3. no phantom suite  -- no wired file is a library, so nothing can enter the
#                           run as a silent pass;
#   4. one suite list    -- the justfile takes its suite set from make rather
#                           than globbing its own, so the two front ends cannot
#                           come to disagree about what "the tests" are;
#   5. no target clash    -- no suite's basename collides with a real Makefile
#                           target, so a suite cannot be shadowed by an explicit
#                           recipe (make would run that recipe, not the suite,
#                           and exit 0) and silently stop running.
#
# What the build RUNS is make's answer, not ours: the recipe hands the two lists
# in through the environment, and a bare invocation asks make for them. They are
# never re-derived here. Two implementations of one discovery rule is precisely
# the drift that produced the defect above, and a gate that carried its own copy
# of the glob would agree with itself while the build ran something else. What
# the tree CONTAINS is this script's own walk. That asymmetry -- an independent
# view of the files, an authoritative view of the run -- is what gives direction
# 1 its teeth.
#
# Because the lists are handed in, the audit runs no BUILD sub-make: no recipe
# executes, so no recursion and no jobserver contention. Direction 5 does query
# make's database with `make -n` (a dry run -- it reads the rules and executes
# nothing), which is why the gate stays safe INSIDE the `test:` aggregate, where
# the standalone .sh proofs -- which run make to build things -- are not.
#
# Scans the directory given as argument one (default test), and honours the
# WIRING_TARGETS / WIRING_HARNESS / JUSTFILE overrides, so a teeth test can point
# the whole audit at a fixture tree. Exits 0 when every invariant holds, 1
# (naming the offender) otherwise.

set -eu

TEST_ROOT="${1:-test}"
MAKE="${MAKE:-make}"
JUSTFILE="${JUSTFILE:-justfile}"
status=0

fail() {
	printf 'check-test-wiring: %s\n' "$1" >&2
	status=1
}

# The suite list and the harness list belong to the build, so ask the build for
# them. The recipe sets WIRING_TARGETS/WIRING_HARNESS directly (no sub-make); a
# by-hand run falls back to make's own published lists. If neither is available
# there is nothing authoritative to audit against -- and inventing a glob here to
# carry on regardless would rebuild the very duplication this gate exists to
# prevent -- so refuse to run rather than audit the tree against a guess.
#
# --no-print-directory is not a nicety. make turns on -w of its own accord in a
# sub-make, and MAKELEVEL travels in the environment -- so the moment this script
# is run from inside any make recipe, a plain `make print-test-targets` prefixes
# its STDOUT with "make[1]: Entering directory ..." and the list read back is
# nonsense: real suites then look unwired, and the words "Entering" and
# "directory" arrive as harness file names. Suppress the chatter at its source
# rather than filtering it back out afterwards. (Jobserver grumbling goes to
# stderr, which is already discarded.)
if [ -n "${WIRING_TARGETS+set}" ]; then
	targets="$WIRING_TARGETS"
elif targets="$("$MAKE" --no-print-directory print-test-targets 2>/dev/null)"; then
	:
else
	printf 'check-test-wiring: cannot read the suite list: WIRING_TARGETS is unset and `%s print-test-targets` failed. The list of suites the build runs has exactly one home; this audit will not guess at it.\n' "$MAKE" >&2
	exit 1
fi

if [ -n "${WIRING_HARNESS+set}" ]; then
	harness="$WIRING_HARNESS"
elif harness="$("$MAKE" --no-print-directory print-test-harness 2>/dev/null)"; then
	:
else
	printf 'check-test-wiring: cannot read the harness list: WIRING_HARNESS is unset and `%s print-test-harness` failed.\n' "$MAKE" >&2
	exit 1
fi

# Is FILE a Scheme library rather than a runnable suite?
#
# The distinction is settled by the file's first datum, so read it with the
# Scheme reader and ask whether it is a (library ...) form. The verdict for
# every file is computed in one reader pass by classify_heads (below); this is
# the lookup into its result.
#
# Reading the DATUM, not matching the first line, is what makes the test sound.
# A first-line match has to re-implement the reader, badly. It must know that
# almost every suite here opens with
#
#     (library-directories '(("src" . "src") ("." . ".")))
#
# which begins with the letters `library` yet is no library at all; and it must
# still see through a `#!chezscheme` directive, a `#| ... |#` block-comment
# header, a `#;` datum comment, an indented form, and a `(library` keyword left
# alone on its opening line. Every one of those is legal Scheme, and a first-line
# regex gets each wrong in one direction or the other -- a real library waved
# through as a phantom suite, or a real suite condemned and the whole build taken
# down with it. The reader settles all of them for free.
#
# Nor be tempted to key this on the assertion harness instead -- on whether the
# file opens a suite by calling into (test runner). At least one suite here uses
# a hand-rolled assertion macro, never calls into the shared harness at all, and
# still exits non-zero on failure exactly as it should; that rule would condemn
# it. Whether a file is a library is a property of the file, not of the harness
# it happens to prefer, and the library head is the only sound discriminator.
is_library() {
	printf '%s\n' "$library_heads" | grep -Fxq -- "$1"
}

# Whole-token membership, never a substring: `poll` must not be found inside
# `test-poll-vacuity`, nor `vterm` inside `test-vterm`.
contains_token() {
	case " $1 " in
	*" $2 "*) return 0 ;;
	*) return 1 ;;
	esac
}

count_tokens() {
	# shellcheck disable=SC2086
	set -- $1
	printf '%d\n' "$#"
}

# The harness list carries paths; compare on the file name so the audit is
# indifferent to how the test root was spelled on the command line.
harness_names=""
for h in $harness; do
	harness_names="$harness_names ${h##*/}"
done

# Classify, with the Scheme reader, every file the suite/library split will ask
# about: the wired suites that have a Scheme file behind them, and the declared
# harness libraries that exist. See the is_library note above for why the head
# datum -- not a first-line match -- is the only sound discriminator. One reader
# process handles the whole batch; an interpreter start-up per file, over the
# ~120 files here, would be too slow to sit in front of every `make test`.
#
# The interpreter is the one the build selected ($SCHEME, threaded in exactly as
# it is for the suites themselves), falling back to `scheme` on PATH. This is a
# Scheme sub-process, not a sub-make -- no recursion, no jobserver -- so it is
# still safe inside the `test:` aggregate. If no interpreter will run, the split
# cannot be read at all: refuse loudly rather than wave every file through
# unclassified and call the tree clean.
SCHEME_BIN="${SCHEME:-scheme}"
classifier="$(mktemp 2>/dev/null)" || {
	printf 'check-test-wiring: cannot create a temporary file for the head-datum classifier.\n' >&2
	exit 1
}
trap 'rm -f "$classifier"' EXIT INT TERM HUP
cat > "$classifier" <<'SCM'
;; Print, per path given as an argument, `library <path>` or `other <path>`,
;; deciding on the file's first datum. A read error or EOF (an empty or
;; malformed file) is reported as `other`: it is simply not a library.
(define (strip-unix-shebang s)
  ;; Chez's --script loader skips a leading `#!` line when the character after
  ;; `!` is `/` or a space (an interpreter path); `#!chezscheme` / `#!r6rs` (a
  ;; letter follows) are reader directives, not shebangs, and are read normally.
  ;; The raw reader does no such skipping, so mirror the loader's rule here.
  (if (and (>= (string-length s) 3)
           (char=? (string-ref s 0) #\#)
           (char=? (string-ref s 1) #\!)
           (memv (string-ref s 2) '(#\/ #\space)))
      (let ([nl (let scan ([i 0])
                  (cond [(>= i (string-length s)) #f]
                        [(char=? (string-ref s i) #\newline) i]
                        [else (scan (+ i 1))]))])
        (if nl (substring s (+ nl 1) (string-length s)) ""))
      s))
(define (head-datum path)
  (let ([s (guard (e [#t #f]) (call-with-input-file path get-string-all))])
    (if (string? s)
        (guard (e [#t (eof-object)])
          (read (open-input-string (strip-unix-shebang s))))
        (eof-object))))
(for-each
  (lambda (path)
    (let ([d (head-datum path)])
      (display (if (and (pair? d) (eq? (car d) 'library)) "library " "other "))
      (display path)
      (newline)))
  (command-line-arguments))
SCM

# The union of files either direction will classify. A name in the suite list
# with no Scheme file behind it is a shell-backed proof (direction 3 skips it);
# a harness name with no file is caught by direction 2, not here.
classify_paths=""
for t in $targets; do
	tf="$TEST_ROOT/$t.ss"
	if [ -f "$tf" ]; then classify_paths="$classify_paths $tf"; fi
done
for h in $harness; do
	if [ -f "$h" ]; then classify_paths="$classify_paths $h"; fi
done

library_heads=""
if [ -n "$classify_paths" ]; then
	# shellcheck disable=SC2086
	classify_out="$("$SCHEME_BIN" -q --script "$classifier" $classify_paths 2>/dev/null)" \
		|| classify_out=""
	# One verdict line per input path. Fewer means the interpreter did not run to
	# the end -- a missing or broken Scheme, say -- and the split is unknown. A
	# gate that could not read the tree must not report it clean.
	want=0
	for p in $classify_paths; do want=$((want + 1)); done
	got="$(printf '%s\n' "$classify_out" | grep -c . || true)"
	if [ "$want" -ne "$got" ]; then
		printf 'check-test-wiring: could not classify the test files: `%s` returned %s of %s head-datum verdicts. The library/suite split cannot be read without a working Scheme, and this gate will not pass a tree it could not check.\n' \
			"$SCHEME_BIN" "$got" "$want" >&2
		exit 1
	fi
	library_heads="$(printf '%s\n' "$classify_out" | sed -n 's/^library //p')"
fi

# 1. No unwired suite. Every .ss file in the directory is either wired into the
#    run or declared a harness library. A file in neither is executed by nothing,
#    and until now nothing said so -- it simply never ran.
files=0
for f in "$TEST_ROOT"/*.ss; do
	[ -e "$f" ] || continue
	files=$((files + 1))
	base="${f##*/}"
	stem="${base%.ss}"
	# Explicit `if`s rather than `cmd && continue`: under set -e the exit status
	# of a short-circuited AND-list is a trap not worth stepping near in a gate
	# whose whole job is to be trustworthy when it fails.
	if contains_token "$targets" "$stem"; then
		continue
	fi
	if contains_token "$harness_names" "$base"; then
		continue
	fi
	fail "$f is wired into nothing: it is neither a suite the build runs nor a declared harness library, so it never executes. Name it in the suite list or hold it out as harness."
done

# 2. No silenced suite. Everything held out of the run must genuinely BE a
#    harness library. The exclusion list is the obvious place to quiet an
#    inconvenient failure, so it is not enough that a name appears there -- the
#    file behind the name has to be the kind of thing the list is for.
for h in $harness; do
	if [ ! -f "$h" ]; then
		fail "$h is named as a harness library but no such file exists -- drop the stale name from the exclusion list."
		continue
	fi
	if ! is_library "$h"; then
		fail "$h is held out of the run as a harness library, but it is not one: its first form is not (library ...). A suite cannot be silenced by parking its name in the exclusion list."
	fi
done

# 3. No phantom suite. Nothing the build RUNS may be a library. Chez defines a
#    (library ...) form handed to --script and exits 0 without printing a word,
#    so such a file does not fail the build -- it passes it, having asserted
#    nothing at all. This is the direction that catches a silent green.
for t in $targets; do
	f="$TEST_ROOT/$t.ss"
	# A target with no Scheme file behind it is one of the shell-backed proofs.
	# Those have their own explicit rules and are not this audit's business.
	[ -f "$f" ] || continue
	if is_library "$f"; then
		fail "$f is run as a suite but is a (library ...) form. Chez would define it and exit 0 having printed nothing, so it would join the run as a suite that asserts nothing and always passes. Hold it out as harness instead of wiring it in."
	fi
done

# 4. One suite list. The other front end must take its suite set from make rather
#    than glob for suites itself. Two globs are two chances to disagree, and the
#    one that finds fewer suites just runs fewer -- silently. A stripped release
#    tarball ships no justfile; there is then no second front end to disagree
#    with, so the direction simply does not apply.
if [ -f "$JUSTFILE" ]; then
	if ! grep -q 'print-test-targets' "$JUSTFILE"; then
		fail "$JUSTFILE no longer takes its suite list from make (nothing in it mentions print-test-targets), so the two front ends can now disagree about which suites are the tests."
	fi
	globs=$(grep -nE 'test/[^[:space:]]*\*[^[:space:]]*\.ss' "$JUSTFILE" || true)
	if [ -n "$globs" ]; then
		fail "$JUSTFILE globs for suites on its own account -- ask make for the list instead:"
		printf '%s\n' "$globs" >&2
	fi
fi

# 5. No target clash. Every discovered suite becomes a make target named for its
#    file's basename (test/scsh-tty.ss -> `scsh-tty`), because the build finds
#    them with a static pattern rule over a bare stem. So a suite whose basename
#    equals one of the build's own targets -- `clean`, `install`, `test-load`,
#    ... -- is shadowed by that target's explicit recipe: make runs the recipe
#    in the suite's place, exits 0, and the suite never runs (and a `clean` or
#    `install` recipe fires its side effects mid-test). Directions 1-4 are blind
#    to it -- the name is in the suite list, its file exists and is not a
#    library, so all four pass -- which is exactly the "a suite silently stops
#    running" failure this gate exists to abolish, in a form none of them sees.
#
#    Ask make itself which names are already targets, never a hand-kept list that
#    would rot. TEST_TARGETS is overridden to a lone sentinel so the suite
#    targets drop out of the dump (they are only there via that static-pattern
#    rule) and just the build's OWN explicit targets remain. `-n` makes it a dry
#    run -- it reads the rule database and executes nothing -- and
#    --no-print-directory stops an inherited MAKELEVEL prefixing the dump with
#    "make[1]: Entering directory ...", the same chatter the suite-list read
#    above suppresses. If make cannot be read the check is skipped, not guessed:
#    a bare tree with no make cannot author this collision in the first place.
reserved="$("$MAKE" --no-print-directory -rpn TEST_TARGETS=__wiring_reserved_probe__ \
	print-test-harness 2>/dev/null | awk '
		/^# Files/ { infiles = 1; next }
		/^# VPATH/ || /^# Finished/ { infiles = 0 }
		infiles && /^# Not a target:/ { nt = 1; next }
		infiles && /^[A-Za-z0-9.\/@+-][^:=]*:/ {
			name = $0; sub(/:.*/, "", name)
			if (!nt) print name
			nt = 0; next
		}
		{ nt = 0 }
	' | grep -v '^__wiring_reserved_probe__$' || true)"
if [ -n "$reserved" ]; then
	reserved_tokens="$(printf '%s' "$reserved" | tr '\n' ' ')"
	for t in $targets; do
		if contains_token "$reserved_tokens" "$t"; then
			fail "$t is wired as a suite (test/$t.ss) but is also a real Makefile target. The suite target and the explicit '$t' recipe collide; make runs the recipe in the suite's place and exits 0, so the suite would silently never run. Rename the suite so its basename does not collide with a build target."
		fi
	done
fi

if [ "$status" -eq 0 ]; then
	printf 'check-test-wiring: test wiring holds (%s: %d files = %d suites wired + %d harness libraries; no unwired suite, no silenced suite, no library passing silently as a suite, one suite list across both front ends, no suite clashing with a build target)\n' \
		"$TEST_ROOT" "$files" "$(count_tokens "$targets")" "$(count_tokens "$harness")"
fi
exit "$status"
