#!/usr/bin/env bash
#
# build.sh -- build and run
#
set -ex

echo build
time stack --system-ghc build

echo run
time stack --system-ghc exec bifrost
