#!/bin/sh -l

set -e

pwd

uname -a

make -j4

./v version

du -s .

echo "DONE"
