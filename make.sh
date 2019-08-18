#!/bin/sh
set -e

[ -z "$CC" ] && CC=cc

git clone --depth 1 --quiet https://github.com/vlang/vc.git
"${CC}" -std=gnu11 -w -o v vc/v.c -lm
./v -o v compiler
rm -rf vc
echo "V has been successfully built"
