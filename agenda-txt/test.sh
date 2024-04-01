#!/usr/bin/env bash

set -euxo pipefail

cabal test
cabal build
shelltest --hide-successes --all integration-tests
