#!/bin/sh -l

set -e

pwd

uname -a

make

./v --version

./v test v

echo "DONE"
