#!/usr/local/bin/bash

set -x ## print all commands as they are executed

echo $PATH

echo '----------------------------------------------------------'
freebsd-version
sysctl hw.model
sysctl hw.ncpu
sysctl hw.physmem
sysctl hw.usermem
echo '----------------------------------------------------------'
whoami
pwd
ls -lah
echo '----------------------------------------------------------'
git config --global --add safe.directory /home/runner/work/v/v
git log -n1
echo '----------------------------------------------------------'
cc --version
echo '----------------------------------------------------------'
echo 'Building local V, without optimisations, which are too slow on the emulated ARM VM'
make CFLAGS=
./v symlink
echo '----------------------------------------------------------'
echo 'Build cmd/tools/fast'
(cd cmd/tools/fast && v fast.v) ## && ./fast -clang
echo '----------------------------------------------------------'
echo 'Test the math module'
v test vlib/math
echo '----------------------------------------------------------'
echo 'Test the math module, using only the pure V versions,'
echo '                      without the .c.v overrides'
v -exclude @vlib/math/*.c.v test vlib/math
echo '----------------------------------------------------------'
echo 'Test modules using thirdparty/zip'
v test vlib/compress/
echo '----------------------------------------------------------'
echo 'Run test-self'
VTEST_JUST_ESSENTIAL=1 ./v test-self
