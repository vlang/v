#!/bin/sh -l

set -e

pwd

uname -a

make

./v --version

du -s .

echo "DONE"
