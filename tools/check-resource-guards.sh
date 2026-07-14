#!/bin/sh
# check-resource-guards.sh -- structural audit of foreign-memory release inside
# error handlers in the POSIX FFI layer.
#
# The failure it guards against: a wrapper frees a foreign block inline and then
# raises, while the surrounding (guard ...) handler frees the SAME block again on
# its way to re-raising. posix_spawn_file_actions_t is an opaque pointer to a
# heap block on some platforms, so the second destroy/free hands the allocator an
# already-freed allocation and the process aborts. That exact shape hid in the
# spawn wrapper across several releases before it was corrected.
#
# The invariant: in internal/posix-*.ss a (guard ...) HANDLER must not contain a
# bare foreign-memory release -- foreign-free, free-c-argv, c-spawn-fa-destroy,
# c-globfree or globfree. Such a release is safe only where a single owner runs
# it: a dynamic-wind after-thunk, or an idempotent latched release the handler
# merely calls through a closure. A descriptor close (posix-close) in a handler
# is legitimate and is deliberately NOT flagged -- this targets memory frees.
#
# The three control-flow cleanup sites in internal/posix-*.ss, recorded with a
# safe/unsafe disposition:
#
#   * posix-spawnp* (posix-core.ss) owns foreign memory -- a pid buffer, the
#     argument and environment vectors, and the spawn file-actions block. Its
#     handler calls one latched release closure; the body calls the SAME closure
#     inline before it raises. The latch collapses the two calls to a single real
#     free, so each block is released exactly once however the wrapper leaves.
#     SAFE -- this is the reference shape the gate is written around.
#
#   * posix-spawnp/pipe (posix-core.ss) closes only descriptors in its handler,
#     never foreign memory. Its worst case is a repeated close of an
#     already-closed descriptor, which returns EBADF and cannot abort the
#     allocator. SAFE (a minor benign double-close only) -- left unchanged; it is
#     not the memory-free shape this gate targets.
#
#   * posix-glob-fast (posix-misc.ss) frees its glob buffer only in a dynamic-wind
#     after-thunk, with no inline free in the body, so a single owner runs the
#     release. SAFE -- a dynamic-wind after-thunk, not a guard handler, and so
#     correctly outside what this gate flags.
#
# The gate keys on precisely that distinction: a BARE free in a re-raising handler
# (banned) versus a latched or after-thunk free (allowed). A verified-safe future
# site may carry a co-located `resource-guard-allow` marker on the offending line
# to exempt it.
#
# Scans SRC_ROOT (default src/hafod/internal) for posix-*.ss so a teeth test can
# point it at a private fixture tree. Exits 0 when the invariant holds, 1 (naming
# the offender as file:line) otherwise.
#
# Copyright (c) 2026 Dominic Pearson.

set -eu

SRC_ROOT="${1:-src/hafod/internal}"
status=0

fail() {
	printf 'check-resource-guards: %s\n' "$1" >&2
	status=1
}

# A bounded region scan, not a single-token grep: the offending shape spans the
# guard handler, so a plain line grep cannot express "bare free INSIDE a guard
# handler". This walks each file character by character, tracks bracket depth
# from every guard form -- (guard or the equivalent [guard -- and flags a banned
# memory-free token only while inside that guard's handler clause, the first
# bracketed child of the guard form.
# Frees in the guard BODY, in a sibling dynamic-wind after-thunk, or routed
# through a latched (release!) are outside the handler clause and are not flagged.
# Strings, line comments and #\char literals are skipped so their brackets never
# skew the depth count. A `resource-guard-allow` marker in a line comment exempts it.
scan_one() {
	awk '
	function flush_token() {
		if (tok == "guard" && (tok_lead == "(" || tok_lead == "[")) {
			gsp += 1
			gdepth[gsp] = depth
			hstate[gsp] = 0
		} else if (gsp > 0 && hstate[gsp] == 1 && (tok in banned)) {
			offense = 1
			offense_line = FILENAME ":" FNR ": " line
		}
		tok = ""
	}
	BEGIN {
		banned["foreign-free"] = 1
		banned["free-c-argv"] = 1
		banned["c-spawn-fa-destroy"] = 1
		banned["c-globfree"] = 1
		banned["globfree"] = 1
	}
	FNR == 1 { depth = 0; gsp = 0; instring = 0; lastsig = ""; tok = "" }
	{
		line = $0
		allow = 0
		offense = 0
		n = length(line)
		i = 1
		while (i <= n) {
			c = substr(line, i, 1)

			if (instring) {
				if (c == "\\") { i += 2; continue }
				if (c == "\"") { instring = 0 }
				i += 1
				continue
			}

			# #\char literal -- skip the escaped character so #\( etc. never
			# perturb the bracket count.
			if (c == "#" && substr(line, i + 1, 1) == "\\") {
				if (tok != "") flush_token()
				i += 3
				continue
			}

			# Line comment: nothing after ; affects depth or tokens. The allow
			# marker is honoured only here, in the comment tail -- never in a
			# string literal or in bare code earlier on the line.
			if (c == ";") {
				if (index(substr(line, i), "resource-guard-allow") > 0) allow = 1
				break
			}

			if (c == "\"") {
				if (tok != "") flush_token()
				instring = 1
				lastsig = c
				i += 1
				continue
			}

			# Scheme identifier characters.
			if (c ~ /[A-Za-z0-9_!?*+.<>=\/-]/) {
				if (tok == "") tok_lead = lastsig
				tok = tok c
				lastsig = c
				i += 1
				continue
			}

			# Any other character ends the current token.
			if (tok != "") flush_token()

			if (c == "(" || c == "[") {
				# The first bracket opened at the guard depth begins its
				# handler clause.
				if (gsp > 0 && hstate[gsp] == 0 && depth == gdepth[gsp]) hstate[gsp] = 1
				depth += 1
				lastsig = c
			} else if (c == ")" || c == "]") {
				depth -= 1
				if (gsp > 0) {
					# Handler clause closed -> the rest of the guard is its body.
					if (hstate[gsp] == 1 && depth == gdepth[gsp]) hstate[gsp] = 2
					# Guard form closed -> pop it.
					if (depth == gdepth[gsp] - 1) gsp -= 1
				}
				lastsig = c
			} else if (c != " " && c != "\t") {
				lastsig = c
			}
			i += 1
		}
		if (tok != "") flush_token()
		if (offense == 1 && allow == 0) print offense_line
	}
	' "$1"
}

offenders=$(
	find "$SRC_ROOT" -name 'posix-*.ss' 2>/dev/null | sort | while IFS= read -r f; do
		scan_one "$f"
	done
)

if [ -n "$offenders" ]; then
	fail "a bare foreign-memory release lives inside a re-raising guard handler in internal/posix-*.ss -- route it through a single-owner dynamic-wind after-thunk or an idempotent latched release:"
	printf '%s\n' "$offenders" >&2
fi

if [ "$status" -eq 0 ]; then
	printf 'check-resource-guards: no bare foreign-memory release inside a re-raising guard handler in internal/posix-*.ss\n'
fi
exit "$status"
