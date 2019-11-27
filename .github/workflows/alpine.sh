#!/bin/sh -l

set -e

pwd

make

/opt/vlang/v test v

echo "DONE"
