#!/usr/bin/env bash
# Run juju's pure parser tests. Requires `steel` on PATH and must run from the
# repository root so module requires resolve.
set -e
cd "$(dirname "$0")/.."
steel < tests/parser-tests.scm
