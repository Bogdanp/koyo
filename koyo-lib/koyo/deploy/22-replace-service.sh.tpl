#lang scribble/text
#!/usr/bin/env bash

set -euo pipefail

sudo chown -R @|user|:@|group| @destination
sudo systemctl start @new-svc
@(define target-port (hash-ref ports target-variant))
@(define target-backend (format "127.0.0.1:~a" target-port))
@(define backends-path (build-path destination "backends.conf"))
@(define exe-path (build-path version-path "bin" exec-name))
@(when health-check?
   @list{
     if ! @exe-path -c @|target-port|; then
       sudo systemctl stop @new-svc
       exit 1
     fi
   })
echo "server @|target-backend|;" >@backends-path
sudo systemctl reload nginx
if [ -f @|variant-path| ]; then
  sudo systemctl stop @old-svc
fi
echo @target-variant >@variant-path
