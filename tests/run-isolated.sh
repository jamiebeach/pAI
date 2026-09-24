#!/bin/sh
# run-isolated.sh -- run each suite in its own process.
#
# The inherited suites were written to run standalone: each loads its own
# subject and was qualified that way. Running them together in one image
# after loading the whole system lets them redefine each other's functions
# and hit gates that only exist under full load. That produced a false
# regression report (see docs/gotchas.md 7), so isolation is not a nicety.
#
# One process per suite is slower and correct. The compile cache makes
# repeat runs cheap.
#
# Usage:
#   tests/run-isolated.sh <sbcl> <repo-root> [suite-glob]
#
# Environment:
#   PAI_TEST_STATE   scratch directory for suites that write (default /tmp)
#   PAI_QUICKLISP_SETUP   optional Quicklisp setup file
#   PAI_TEST_LOG_DIR      optional directory for complete per-suite output
#
# Exit status counts failed and unqualified suites, capped at 125. An empty
# selection also fails. A passing tally cannot override a later abort.

set -u

SBCL="${1:-sbcl}"
ROOT="${2:-/pai}"
GLOB="${3:-*-tests.lisp}"
PAI_ROOT="$ROOT/"
export PAI_ROOT

PAI_TEST_STATE_ROOT="${PAI_TEST_STATE:-/tmp/pai-test-state}"
mkdir -p "$PAI_TEST_STATE_ROOT"

pass=0; fail=0; err=0; total=0
failed_names=""; err_names=""

QUICKLISP="${PAI_QUICKLISP_SETUP:-$ROOT/.tools/quicklisp/setup.lisp}"

for suite in "$ROOT"/tests/$GLOB; do
  [ -f "$suite" ] || continue
  name=$(basename "$suite")

  case "$name" in
    helpers.lisp|run-all.lisp|isolated-harness.lisp) continue ;;
  esac

  total=$((total + 1))
  suite_state="$PAI_TEST_STATE_ROOT/$name"
  mkdir -p "$suite_state"

  if [ -f "$QUICKLISP" ]; then
    out=$(SUITE="$suite" PAI_TEST_STATE="$suite_state" "$SBCL" --dynamic-space-size 3072 --non-interactive \
            --load "$QUICKLISP" --eval '(require :asdf)' \
            --load "$ROOT/tests/isolated-harness.lisp" 2>&1)
  else
    out=$(SUITE="$suite" PAI_TEST_STATE="$suite_state" "$SBCL" --dynamic-space-size 3072 --non-interactive \
            --eval '(require :asdf)' \
            --load "$ROOT/tests/isolated-harness.lisp" 2>&1)
  fi
  status=$?

  if [ -n "${PAI_TEST_LOG_DIR:-}" ]; then
    mkdir -p "$PAI_TEST_LOG_DIR"
    printf '%s\n' "$out" > "$PAI_TEST_LOG_DIR/$name.log"
  fi

  # A positive tally is evidence only after abort/failure checks. Inspect all
  # output so a later passing summary cannot conceal an earlier failed check.
  tally=$(printf '%s\n' "$out" | grep -oE '[0-9]+ passed, [0-9]+ failed' | tail -1)

  if printf '%s\n' "$out" | grep -q 'HARNESS-ERR'; then
    err=$((err + 1)); err_names="$err_names $name"
    reason=$(printf '%s\n' "$out" | grep -oE 'HARNESS-ERR: .{0,70}' | tail -1)
    printf 'ERR   %-52s %s\n' "$name" "${reason:-harness aborted; exit $status}"
  elif printf '%s\n' "$out" | grep -qE '^[[:space:]]*(FAIL|[A-Z0-9_]+_FAIL)([[:space:]:]|$)|(^|[^0-9/])[1-9][0-9]* failed'; then
    fail=$((fail + 1)); failed_names="$failed_names $name"
    printf 'FAIL  %-52s %s\n' "$name" 'failure reported in suite output'
    printf '%s\n' "$out" | grep -E '^[[:space:]]*FAIL|[0-9]+ failed' | sed 's/^/        /'
  elif [ "$status" -ne 0 ]; then
    err=$((err + 1)); err_names="$err_names $name"
    printf 'ERR   %-52s process aborted; exit %s\n' "$name" "$status"
  elif [ -n "$tally" ] && [ "$tally" != '0 passed, 0 failed' ]; then
    f=$(printf '%s' "$tally" | sed -E 's/.*, ([0-9]+) failed/\1/')
    if [ "$f" -gt 0 ] 2>/dev/null; then
      fail=$((fail + 1)); failed_names="$failed_names $name"
      printf 'FAIL  %-52s %s\n' "$name" "$tally"
      printf '%s\n' "$out" | grep -E '^\s*FAIL' | sed 's/^/        /'
    else
      pass=$((pass + 1))
      printf 'ok    %-52s %s\n' "$name" "$tally"
    fi
  elif [ "$tally" != '0 passed, 0 failed' ] && [ "$status" -eq 0 ] \
       && ! printf '%s\n' "$out" | grep -q 'HARNESS-ERR' \
       && printf '%s\n' "$out" | grep -qE '^[[:space:]]*(ok|PASS|[A-Z0-9_]+_PASS)([[:space:]:]|$)'; then
    # Assert-by-raising suites: they print progress lines and call ERROR on a
    # failed check rather than counting, so they never emit a tally. Exit 0
    # from one of these is a real pass.
    #
    # All three conditions are required, and the HARNESS-ERR check is the one
    # that actually matters. The harness traps the abort and returns normally,
    # so a suite that printed a few ok lines and then died still exits 0 --
    # without this guard those get counted as passing, which is worse than the
    # gap it was meant to close. Exit status alone would likewise cover a suite
    # that loaded, did nothing and returned, so at least one satisfied check
    # must also be on record.
    checks=$(printf '%s\n' "$out" | grep -cE '^[[:space:]]*(ok|PASS|[A-Z0-9_]+_PASS)([[:space:]:]|$)')
    pass=$((pass + 1))
    printf 'ok    %-52s %s checks, no tally (raises on failure)\n' "$name" "$checks"
  else
    err=$((err + 1)); err_names="$err_names $name"
    reason=$(printf '%s\n' "$out" | grep -oE 'HARNESS-ERR: .{0,70}' | tail -1)
    [ -z "$reason" ] && reason=$(printf '%s\n' "$out" | grep -oE '(does not exist|Connection refused|getaddrinfo|is undefined|is unbound).{0,50}' | tail -1)
    printf 'ERR   %-52s %s\n' "$name" "${reason:-no tally reported; exit $status}"
  fi
done

if [ "$total" -eq 0 ]; then
  err=$((err + 1))
  echo 'ERR   no suites matched the requested selection'
fi

echo
echo "----------------------------------------------------------------"
printf 'suites            %4d\n' "$total"
printf 'passed            %4d\n' "$pass"
printf 'failed            %4d\n' "$fail"
printf 'could not run     %4d\n' "$err"
echo "----------------------------------------------------------------"

[ "$fail" -gt 0 ] && echo "failed:$failed_names"
[ "$err" -gt 0 ] && echo "unqualified:$err_names"
unsatisfied=$((fail + err))
exit $((unsatisfied > 125 ? 125 : unsatisfied))
