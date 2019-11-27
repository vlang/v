#!/bin/sh -l

set -e

echo "Hello from the Alpine docker image."
echo "Argument 1: $1"
echo "Argument @: $@"

make

v test v



