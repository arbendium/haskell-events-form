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
