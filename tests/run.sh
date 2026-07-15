#!/usr/bin/env bash
# Run juju's pure unit suites. Each tests/test-*.scm is a steel-test file run
# in file mode: it ends in (run-tests!), which raises on any failure or error,
# so file mode exits nonzero. The exit code is the verdict; this script runs
# every suite and aggregates them.
#
# Requires `steel` on PATH and must run from the repository root so the
# file-relative requires (../cogs/juju/...) resolve. juju's modules require the
# shared ui-utils.hx library and the run-command cog from ~/.steel/cogs, and
# the suites require the steel-test package there, so all three must be
# installed first.
set -u
cd "$(dirname "$0")/.."

for dep in ui-utils.hx run-command steel-test; do
    if [ ! -d "$HOME/.steel/cogs/$dep" ]; then
        echo "$dep is not installed in ~/.steel/cogs; install it first" >&2
        exit 1
    fi
done

fail=0
for t in tests/test-*.scm; do
    if steel "$t" >/dev/null 2>&1; then
        echo "PASS $t"
    else
        echo "FAIL $t"
        steel "$t" 2>&1 | sed 's/^/    /'
        fail=1
    fi
done
exit $fail
