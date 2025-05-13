#lang scribble/text
#!/usr/bin/env bash

set -euo pipefail

ln -sfT @version-path @|versions-path|/current
ln -sfT @version-path @|versions-path|/current-@target-variant
