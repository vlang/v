#!/bin/sh -l

set -e

pwd

uname -a

du -s .

ls -lat 

##./v -silent test-all

## try running the known failing tests first to get faster feedback
./v vlib/builtin/string_test.v
./v vlib/strings/builder_test.v

./v -silent test-cleancode

./v -silent test-self

./v build-vbinaries

echo "DONE"
