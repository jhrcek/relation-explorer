#!/bin/env bash
set -euxo pipefail
COMMIT=$(git rev-parse HEAD)
make build
rm -rf /tmp/relation-explorer
cp -r dist/. /tmp/relation-explorer
git checkout gh-pages
rm -rf ./*
cp -r /tmp/relation-explorer/. docs/
git add .
git commit -m "Deploy ${COMMIT}"
