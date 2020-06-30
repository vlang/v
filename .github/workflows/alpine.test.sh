#!/bin/sh -l

set -e

pwd

uname -a

du -s .

ls -lat 

##./v test-compiler

./v test-fixed

./v build-vbinaries

echo "DONE"
