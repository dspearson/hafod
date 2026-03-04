#!/usr/bin/env bash
# tools/coverage-audit.sh -- API Coverage Audit for hafod
# Extracts all exported symbols from the umbrella library and sub-libraries,
# cross-references with test files, and generates a structured coverage report.
#
# Usage: bash tools/coverage-audit.sh
# Output: .planning/phases/19-api-coverage-audit/COVERAGE-REPORT.md

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$PROJECT_ROOT"

REPORT_DIR=".planning/phases/19-api-coverage-audit"
REPORT_FILE="$REPORT_DIR/COVERAGE-REPORT.md"
TMPDIR_AUDIT=$(mktemp -d)
trap 'rm -rf "$TMPDIR_AUDIT"' EXIT

mkdir -p "$REPORT_DIR"

# ============================================================
# Step 1: Extract exports from the umbrella (hafod.ss)
# The umbrella has section comments like:
#   ;; === (hafod compat) ===
# followed by symbol names, one or more per line
# ============================================================

echo "Extracting umbrella exports..."

# Use awk to parse the umbrella export block
awk '
  # Track when we are inside the (export ...) block
  /^\s*\(export/ { in_export=1; next }
  /^\s*\(import/ { in_export=0; next }

  in_export {
    # Detect section headers like ;; === (hafod compat) ===
    if (match($0, /;; === \(hafod ([^)]+)\) ===/, m)) {
      current_lib = m[1]
      next
    }
    # Skip comment-only lines
    if ($0 ~ /^\s*;;/) next

    # Remove inline comments
    sub(/;;.*$/, "")
    # Remove parens (not part of symbol names in export list)
    gsub(/[()]/, " ")

    # Extract tokens
    n = split($0, tokens, /[[:space:]]+/)
    for (i = 1; i <= n; i++) {
      t = tokens[i]
      if (t == "") continue
      if (t == "export" || t == "rename" || t == "except" || t == "only") continue
      # Valid symbol: starts with letter, colon, &, %, *, !, ?, /, >, <, +, ^, -, |
      if (t ~ /^[a-zA-Z_:&%*!?\/>=<+^|.-][a-zA-Z0-9_:&%*!?\/>=<+^|.-]*$/) {
        print current_lib "\t" t
      }
    }
  }
' src/hafod.ss | sort -t$'\t' -k1,1 -k2,2 > "$TMPDIR_AUDIT/umbrella-exports.tsv"

UMBRELLA_TOTAL=$(wc -l < "$TMPDIR_AUDIT/umbrella-exports.tsv")
echo "  Found $UMBRELLA_TOTAL symbols in umbrella"

# ============================================================
# Step 2: Extract exports from each sub-library
# ============================================================

echo "Extracting sub-library exports..."

extract_lib_exports() {
  local libfile="$1"
  # Derive library name from path: src/hafod/foo.ss -> foo, src/hafod/internal/errno.ss -> internal/errno
  local libname
  libname=$(echo "$libfile" | sed 's|^src/hafod/||; s|\.ss$||')

  awk -v lib="$libname" '
    /^\s*\(export/ { in_export=1; depth=0 }
    in_export {
      for (i=1; i<=length($0); i++) {
        c = substr($0, i, 1)
        if (c == "(") depth++
        if (c == ")") {
          depth--
          if (depth == 0) { in_export=0; break }
        }
      }
      # Remove comments and parens
      line = $0
      sub(/;;.*$/, "", line)
      gsub(/[()]/, " ", line)
      n = split(line, tokens, /[[:space:]]+/)
      for (j = 1; j <= n; j++) {
        t = tokens[j]
        if (t == "") continue
        if (t == "export" || t == "library" || t == "rename" || t == "except" || t == "only") continue
        if (t ~ /^[a-zA-Z_:&%*!?\/>=<+^|.-][a-zA-Z0-9_:&%*!?\/>=<+^|.-]*$/) {
          print lib "\t" t
        }
      }
    }
  ' "$libfile"
}

> "$TMPDIR_AUDIT/sublib-exports.tsv"

for f in src/hafod/*.ss; do
  extract_lib_exports "$f" >> "$TMPDIR_AUDIT/sublib-exports.tsv"
done

for f in src/hafod/internal/*.ss; do
  extract_lib_exports "$f" >> "$TMPDIR_AUDIT/sublib-exports.tsv"
done

SUBLIB_TOTAL=$(wc -l < "$TMPDIR_AUDIT/sublib-exports.tsv")
echo "  Found $SUBLIB_TOTAL total sub-library exports"

# ============================================================
# Step 3: Cross-reference with test files
# ============================================================

echo "Cross-referencing with test files..."

# Build list of test files (excluding runner)
TEST_FILES=$(ls test/*.ss 2>/dev/null | grep -v runner.ss || true)

# Strategy: build a token index from all test files ONCE, then look up each symbol.
# This avoids O(symbols * test_files) grep invocations.

# For each test file, extract all tokens (Scheme identifiers) and record them
# Format: test-file-basename TAB token
echo "  Building test file token index..."
for tf in $TEST_FILES; do
  bn=$(basename "$tf" .ss)
  # Extract tokens: split on whitespace, parens, quotes, brackets, semicolons
  # Remove comment lines first (lines starting with optional whitespace then ;)
  # Also remove string literals (text between double quotes)
  sed 's/;.*$//' "$tf" | \
    sed 's/"[^"]*"//g' | \
    tr '()[]'"'"'`,#{}' ' ' | \
    tr -s '[:space:]' '\n' | \
    grep -E '^[a-zA-Z_:&%*!?/>=<+^|.-][a-zA-Z0-9_:&%*!?/>=<+^|.-]*$' | \
    sort -u | \
    awk -v file="$bn" '{ print $0 "\t" file }'
done | sort -t$'\t' -k1,1 > "$TMPDIR_AUDIT/test-token-index.tsv"

echo "  Token index built ($(wc -l < "$TMPDIR_AUDIT/test-token-index.tsv") entries)"

# Aggregate the token index: for each token, list all test files (comma-separated)
awk -F'\t' '{
  if ($1 in files) files[$1] = files[$1] ", " $2
  else files[$1] = $2
} END {
  for (sym in files) print sym "\t" files[sym]
}' "$TMPDIR_AUDIT/test-token-index.tsv" | sort > "$TMPDIR_AUDIT/test-token-agg.tsv"

echo "  Aggregated to $(wc -l < "$TMPDIR_AUDIT/test-token-agg.tsv") unique tokens"

# Now join umbrella exports with the aggregated token index
# Use awk for a hash-join (fast, single pass)
awk -F'\t' '
  NR == FNR {
    # Load aggregated test token index into hash
    test_files[$1] = $2
    next
  }
  {
    lib = $1
    sym = $2
    if (sym in test_files) {
      print lib "\t" sym "\tTESTED\t" test_files[sym]
    } else {
      print lib "\t" sym "\tUNTESTED\t-"
    }
  }
' "$TMPDIR_AUDIT/test-token-agg.tsv" "$TMPDIR_AUDIT/umbrella-exports.tsv" > "$TMPDIR_AUDIT/coverage-results.tsv"

total_tested=$(awk -F'\t' '$3 == "TESTED"' "$TMPDIR_AUDIT/coverage-results.tsv" | wc -l)
total_untested=$(awk -F'\t' '$3 == "UNTESTED"' "$TMPDIR_AUDIT/coverage-results.tsv" | wc -l)

echo "  Tested: $total_tested, Untested: $total_untested"

# ============================================================
# Step 4: Generate the coverage report
# ============================================================

echo "Generating coverage report..."

total_symbols=$((total_tested + total_untested))
if [[ $total_symbols -gt 0 ]]; then
  coverage_pct=$((total_tested * 100 / total_symbols))
else
  coverage_pct=0
fi

REPORT_DATE=$(date -u +"%Y-%m-%d %H:%M UTC")

cat > "$REPORT_FILE" << HEADER
# API Coverage Report

Generated: ${REPORT_DATE}
Script: tools/coverage-audit.sh

## Summary

| Metric | Count |
|--------|-------|
| Total exported symbols | ${total_symbols} |
| Tested symbols | ${total_tested} |
| Untested symbols | ${total_untested} |
| Coverage percentage | ${coverage_pct}% |

## Per-Library Coverage

HEADER

# Get unique library names in order of appearance
awk -F'\t' '!seen[$1]++ { print $1 }' "$TMPDIR_AUDIT/coverage-results.tsv" > "$TMPDIR_AUDIT/lib-order.txt"

while IFS= read -r lib; do
  lib_tested=0
  lib_untested=0
  lib_total=0

  while IFS=$'\t' read -r l s status files; do
    ((lib_total++)) || true
    if [[ "$status" == "TESTED" ]]; then
      ((lib_tested++)) || true
    else
      ((lib_untested++)) || true
    fi
  done < <(awk -F'\t' -v lib="$lib" '$1 == lib' "$TMPDIR_AUDIT/coverage-results.tsv")

  if [[ $lib_total -gt 0 ]]; then
    lib_pct=$((lib_tested * 100 / lib_total))
  else
    lib_pct=0
  fi

  {
    echo "### (hafod ${lib})"
    echo ""
    echo "**Coverage: ${lib_tested}/${lib_total} (${lib_pct}%)**"
    echo ""
    echo "| Symbol | Status | Test File(s) |"
    echo "|--------|--------|--------------|"

    while IFS=$'\t' read -r l sym status files; do
      echo "| \`${sym}\` | ${status} | ${files} |"
    done < <(awk -F'\t' -v lib="$lib" '$1 == lib' "$TMPDIR_AUDIT/coverage-results.tsv")

    echo ""
  } >> "$REPORT_FILE"
done < "$TMPDIR_AUDIT/lib-order.txt"

# ============================================================
# Untested Symbols section
# ============================================================

{
  echo "## Untested Symbols"
  echo ""
  echo "Actionable gap list for Phase 24:"
  echo ""
} >> "$REPORT_FILE"

while IFS= read -r lib; do
  untested_syms=$(awk -F'\t' -v lib="$lib" '$1 == lib && $3 == "UNTESTED" { print $2 }' "$TMPDIR_AUDIT/coverage-results.tsv")
  if [[ -n "$untested_syms" ]]; then
    {
      echo "### (hafod ${lib})"
      echo ""
      while read -r sym; do
        echo "- \`${sym}\`"
      done <<< "$untested_syms"
      echo ""
    } >> "$REPORT_FILE"
  fi
done < "$TMPDIR_AUDIT/lib-order.txt"

# ============================================================
# Umbrella Cross-Check
# ============================================================

{
  echo "## Umbrella Cross-Check"
  echo ""
} >> "$REPORT_FILE"

# Check for symbols in umbrella but not in any sub-library
umbrella_only=""
while IFS=$'\t' read -r lib sym; do
  if ! awk -F'\t' -v s="$sym" '$2 == s { found=1; exit } END { exit !found }' "$TMPDIR_AUDIT/sublib-exports.tsv" 2>/dev/null; then
    umbrella_only="${umbrella_only}- \`${sym}\` (umbrella section: hafod ${lib})\n"
  fi
done < "$TMPDIR_AUDIT/umbrella-exports.tsv"

if [[ -n "$umbrella_only" ]]; then
  {
    echo "**Symbols in umbrella but not found in sub-library exports:**"
    echo ""
    printf '%b' "$umbrella_only"
    echo ""
  } >> "$REPORT_FILE"
else
  echo "All umbrella symbols map to sub-library exports. No discrepancies found." >> "$REPORT_FILE"
  echo "" >> "$REPORT_FILE"
fi

# Check for sub-library symbols not in umbrella (excluding internal libraries)
sublib_only=""
while IFS=$'\t' read -r lib sym; do
  # Skip internal libraries -- they are not expected in the umbrella
  case "$lib" in
    internal/*) continue ;;
  esac
  if ! awk -F'\t' -v s="$sym" '$2 == s { found=1; exit } END { exit !found }' "$TMPDIR_AUDIT/umbrella-exports.tsv" 2>/dev/null; then
    sublib_only="${sublib_only}- \`${sym}\` (from hafod ${lib})\n"
  fi
done < "$TMPDIR_AUDIT/sublib-exports.tsv"

if [[ -n "$sublib_only" ]]; then
  {
    echo "**Symbols exported by sub-libraries but not in umbrella (excluding internal):**"
    echo ""
    printf '%b' "$sublib_only"
    echo ""
  } >> "$REPORT_FILE"
else
  echo "All sub-library exports (excluding internal) are included in the umbrella." >> "$REPORT_FILE"
  echo "" >> "$REPORT_FILE"
fi

# ============================================================
# Report Validation section
# ============================================================

{
  echo "## Report Validation"
  echo ""
  echo "- Total symbols in report: ${total_symbols}"
  echo "- Umbrella export count: ${UMBRELLA_TOTAL}"
  echo "- Symbol count match: $(if [[ $total_symbols -eq $UMBRELLA_TOTAL ]]; then echo 'YES'; else echo "NO (delta: $((total_symbols - UMBRELLA_TOTAL)))"; fi)"
  echo "- Sub-library sections present: $(wc -l < "$TMPDIR_AUDIT/lib-order.txt") of 29"
  echo "- Tested symbols: ${total_tested}"
  echo "- Untested symbols: ${total_untested}"
  echo "- Coverage: ${coverage_pct}%"
  echo ""
} >> "$REPORT_FILE"

echo ""
echo "Coverage report generated: $REPORT_FILE"
echo "Total: $total_symbols symbols, $total_tested tested ($coverage_pct%), $total_untested untested"
