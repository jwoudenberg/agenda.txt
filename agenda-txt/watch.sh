#!/usr/bin/env bash

set -euxo pipefail

hpack
ghcid --test Test.main
