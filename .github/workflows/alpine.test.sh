#!/bin/sh -l

set -e

pwd

uname -a

ls -lart 

du -s .

./v -stats test v

echo "DONE"
