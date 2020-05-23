#!/usr/bin/env bash

set -euo pipefail

log() {
    printf "[%s] %s" "$(date)" "$@"
}

log "Making scratch space..."
mkdir -p /tmp/scratch
pushd /tmp/scratch

application="$1-example"
log "Creating app from blueprint..."
raco koyo new --blueprint "$1" "$application"

log "Installing example app and deps..."
pushd "$application"
raco pkg install --batch --auto "$application/" "$application-tests/"

log "Running example app tests..."
raco test "$application-tests/"
