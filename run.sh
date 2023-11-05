#!/usr/bin/env bash

# This is a wrapper for cabal run to use in integration tests.
# If `cabal run` needs to compile first it will write to stdout, which will
# cause tests to fail. This wrapper wil call `cabal build` first, dropping its
# output, so `cabal run` then doesn't need to compile.

set -euo pipefail

cabal build &> /dev/null
cabal run agenda-txt -- "$@"
