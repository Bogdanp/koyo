#!/usr/bin/env bash

set -euo pipefail

log() {
    printf "[%s] %s" "$(date)" "$@"
}

log "Making scratch space..."
mkdir -p /tmp/scratch
pushd /tmp/scratch

appname="$1-example"
log "Creating app from blueprint..."
raco koyo new --blueprint "$1" "$appname"

log "Installing example app and deps..."
pushd "$appname"
raco pkg install --batch --auto "$appname/" "$appname-tests/"

log "Running example app tests..."
raco test "$appname-tests/"
