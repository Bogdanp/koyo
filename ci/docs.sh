#!/usr/bin/env bash

set -euo pipefail

log() {
    printf "[%s] %s" "$(date)" "$@"
}

log "Installing deps from apt..."
sudo apt-get update
sudo apt-get install -y --no-install-recommends gettext-base gpg ssh rsync

log "Building docs..."
raco scribble +m --dest doc --html-tree 2 --redirect-main 'http://docs.racket-lang.org/' koyo-doc/scribblings/koyo.scrbl

log "Decrypting key..."
mkdir -p /tmp/secrets
gpg -q --batch --yes --decrypt --passphrase="$KOYO_DOCS_DEPLOY_KEY_PASSPHRASE" -o /tmp/secrets/deploy ci/deploy.gpg
chmod 0600 /tmp/secrets/deploy

log "Deploying docs..."
rsync \
    -e "ssh -p $KOYO_DOCS_SSH_PORT -o StrictHostKeyChecking=no -i /tmp/secrets/deploy" \
    -a doc/koyo/ koyo@"$KOYO_DOCS_SSH_HOST":~/www/

log "Cleaning up..."
rm /tmp/secrets/deploy
