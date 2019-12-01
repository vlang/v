#!/bin/sh -l

set -e

pwd

uname -a

du -s .

ls -lat 

./v test-compiler

./v build-vbinaries

echo "DONE"
