#!/usr/bin/env bash

set -euo pipefail

apt-get update
apt-get install -y --no-install-recommends gettext-base gpg ssh rsync

pushd /github/workspace
scribble +m --dest doc --html-tree 2 --redirect 'http://docs.racket-lang.org/' koyo-doc/scribblings/koyo.scrbl

mkdir -p /tmp/secrets
gpg -q --batch --yes --decrypt --passphrase="$KOYO_DOCS_DEPLOY_KEY_PASSPHRASE" -o /tmp/secrets/deploy ci/deploy.gpg
chmod 0600 /tmp/secrets/deploy
rsync \
    -e "ssh -p $KOYO_DOCS_SSH_PORT -o StrictHostKeyChecking=no -i /tmp/secrets/deploy" \
    -a doc/koyo/ koyo@"$KOYO_DOCS_SSH_HOST":~/www/
popd
