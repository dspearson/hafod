# hafod -- common developer tasks (https://github.com/casey/just)
#
# Run inside the Nix dev shell (`nix develop` or direnv) so `scheme` resolves to
# the project's pinned Chez and every artefact is built with one version.
# Override the Chez explicitly with e.g. `just scheme=/path/to/scheme build`.

# Chez Scheme binary (auto-detected; "scheme" on most systems, "chez-scheme" on *BSD)
scheme := `command -v scheme 2>/dev/null || command -v chez-scheme 2>/dev/null || echo scheme`

# Install prefix
prefix := "/usr/local"

# Per-test timeout (seconds) for the hang-proofed test runner
test_timeout := "60"

# List available recipes
default:
    @just --list

# Compile all libraries (no native binary)
compile:
    make compile SCHEME={{scheme}}

# Build the native binary (compiles libraries first) -- the default target
build:
    make native SCHEME={{scheme}}

# Alias for `build`
native: build

# Build the self-contained single-file binary
standalone:
    make standalone SCHEME={{scheme}}

# Run the full test suite, hang-proofed (per-test timeout; stdin from /dev/null)
#
# The suite set comes from `make print-test-targets`; this recipe does not glob
# for suites itself. Two globs are two chances to disagree about what "the
# tests" are, and the one that finds fewer suites just runs fewer -- silently.
# So the Makefile owns WHICH suites run, and this file owns the timeout they run
# under (the Makefile parses test_timeout back out of it): each fact has exactly
# one home, and the two front ends cannot drift apart on either.
#
# The wiring audit runs first, ahead of even the compile, exactly as it does in
# `make test`: it is a read-only grep answering in well under a second, and what
# it proves is that the loop below runs the suites it claims to. A gate that only
# one of the two documented ways of running the tests executes is only half a
# gate -- it would be found broken by whoever was not using make. Its `|| exit 1`
# is load-bearing: this recipe runs under `set -u`, not `set -e`, and its exit
# status is the tally on the last line, so an unchecked failure here would be
# reported as a clean run.
test:
    #!/usr/bin/env sh
    set -u
    make --no-print-directory check-test-wiring SCHEME={{scheme}} || exit 1
    make compile SCHEME={{scheme}} >/dev/null
    pass=0; fail=0; hang=0
    for name in $(make --no-print-directory print-test-targets SCHEME={{scheme}}); do
      SCHEME={{scheme}} timeout -k 5 {{test_timeout}} {{scheme}} --libdirs .:src \
        --script "test/$name.ss" </dev/null >"/tmp/hafod-test-$name.log" 2>&1
      rc=$?
      if [ "$rc" -eq 0 ]; then
        pass=$((pass+1)); printf '  PASS  %s\n' "$name"
      elif [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
        hang=$((hang+1)); printf '  HANG  %s (killed after {{test_timeout}}s)\n' "$name"
      else
        fail=$((fail+1)); printf '  FAIL  %s (exit %s)\n' "$name" "$rc"
        tail -n 5 "/tmp/hafod-test-$name.log"
      fi
    done
    if sh test/test-launcher.sh </dev/null >/tmp/hafod-test-launcher.log 2>&1; then
      pass=$((pass+1)); printf '  PASS  test-launcher.sh\n'
    else
      fail=$((fail+1)); printf '  FAIL  test-launcher.sh\n'; tail -n 5 /tmp/hafod-test-launcher.log
    fi
    printf '\n  total: pass=%s fail=%s hang=%s\n' "$pass" "$fail" "$hang"
    [ "$fail" -eq 0 ] && [ "$hang" -eq 0 ]

# Run a single test suite by name, e.g. `just test-one editor`, `just test-one
# scsh-tty`. Both the bare name and the full target name are accepted.
#
# The name is resolved against the authoritative target list, never against the
# filesystem. A filesystem probe would cheerfully run test/vterm.ss -- a library,
# not a suite -- and Chez would define it and exit 0 having printed nothing, so
# `just test-one vterm` would report a silent pass in place of the real
# assertions in test/test-vterm.ss. The target list holds only wired suites, so
# by construction it cannot name a library.
#
# The match is on a whole token, not a substring: a substring match would resolve
# `runner` to test-runner-teeth and `poll` to test-poll-vacuity, quietly running
# an unrelated suite instead of reporting that the name is not a suite at all.
test-one name:
    #!/usr/bin/env sh
    set -u
    targets=$(make --no-print-directory print-test-targets SCHEME={{scheme}})
    suite=""
    for candidate in '{{name}}' 'test-{{name}}'; do
      case " $targets " in
        *" $candidate "*) suite="$candidate"; break ;;
      esac
    done
    if [ -z "$suite" ]; then
      printf 'just: no such test suite: %s\n' '{{name}}' >&2
      exit 1
    fi
    SCHEME={{scheme}} timeout -k 5 {{test_timeout}} {{scheme}} --libdirs .:src \
      --script "test/$suite.ss" </dev/null

# Print the version resolved from git tags (or the VERSION fallback)
version:
    @sh tools/gen-version.sh --print

# Build, then install to {{prefix}} (uses sudo; passes SCHEME so boot files match)
install: build
    sudo make install PREFIX={{prefix}} SCHEME={{scheme}}

# Remove installed files from {{prefix}}
uninstall:
    sudo make uninstall PREFIX={{prefix}}

# Remove all build artefacts
clean:
    make clean

# Launch the REPL from the build tree
repl: compile
    ./bin/hafod
