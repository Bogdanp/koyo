#!/usr/bin/env bash

set -euo pipefail

pushd /github/workspace
raco pkg install --auto --batch koyo-lib/ koyo-doc/ koyo-test/
popd
