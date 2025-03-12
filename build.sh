#!/bin/sh

set -e

if command -v stack >/dev/null
then
  echo 'upgrading haskell stack'
  stack upgrade --binary-only
else
  echo 'installing haskell stack'
  wget -qO- https://get.haskellstack.org/ | sh
fi

stack build

TARGET_DIR="$(readlink -f $(dirname $0))/.build"
rm -rf $TARGET_DIR
mkdir $TARGET_DIR
cp "$(stack path --local-install-root)/bin/events-form" $TARGET_DIR
cp -R templates www $TARGET_DIR
