#!/usr/bin/env bash
# Run juju's pure parser tests. Requires `steel` on PATH and must run from the
# repository root so module requires resolve. juju's modules require the
# shared ui-utils.hx library from ~/.steel/cogs, so it must be installed.
set -e
cd "$(dirname "$0")/.."
if [ ! -d "$HOME/.steel/cogs/ui-utils.hx" ]; then
    echo "ui-utils.hx is not installed; run its install.sh first (e.g. ../ui-utils.hx/install.sh)" >&2
    exit 1
fi
steel < tests/parser-tests.scm
