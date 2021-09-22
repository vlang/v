#!/bin/sh -l

set -e

pwd

uname -a

du -s .

ls -lat 

##./v test-all

## try running the known failing tests first to get faster feedback
./v test vlib/builtin/string_test.v vlib/strings/builder_test.v

./v test-self

./v build-vbinaries

echo "DONE"
