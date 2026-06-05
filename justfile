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
test:
    #!/usr/bin/env sh
    set -u
    make compile SCHEME={{scheme}} >/dev/null
    pass=0; fail=0; hang=0
    for f in test/test-*.ss; do
      name=$(basename "$f" .ss)
      timeout -k 5 {{test_timeout}} {{scheme}} --libdirs .:src --script "$f" </dev/null \
        >"/tmp/hafod-test-$name.log" 2>&1
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

# Run a single test suite by name, e.g. `just test-one editor`
test-one name:
    timeout -k 5 {{test_timeout}} {{scheme}} --libdirs .:src --script test/test-{{name}}.ss </dev/null

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
