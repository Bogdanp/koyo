#lang scribble/text
#!/usr/bin/env bash

set -euo pipefail

cat | sudo tee /etc/systemd/system/@|service-name|

sudo systemctl daemon-reload
