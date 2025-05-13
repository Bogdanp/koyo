#lang scribble/text
#!/usr/bin/env bash

set -euo pipefail

FILENAME="$(mktemp)"
cat | tee "$FILENAME"

cleanup() {
    rm "$FILENAME"
}

trap cleanup exit

bash "$FILENAME" "@current-variant" "@target-variant"
