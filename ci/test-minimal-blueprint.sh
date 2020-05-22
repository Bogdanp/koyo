#!/usr/bin/env bash

set -euo pipefail

log() {
    printf "[%s] %s" "$(date)" "$@"
}

log "Making scratch space..."
mkdir -p /tmp/scratch
pushd /tmp/scratch

log "Creating app from blueprint..."
raco koyo new -b minimal example

log "Installing example app and deps..."
pushd example
raco pkg install --batch --auto example/ example-tests/

log "Running example app tests..."
raco test example-tests/
