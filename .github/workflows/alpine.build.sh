#!/bin/sh -l

set -e

pwd

uname -a

make -j4
V_CI_SOURCE_BUILD=1 sh .github/workflows/prepare_v.sh

./v version

du -s .

echo "DONE"
