#lang scribble/text
#!/usr/bin/env bash

set -euo pipefail

find @versions-path -maxdepth 1 -type d -mtime +30 -exec rm -r {} \;
