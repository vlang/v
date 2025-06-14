#!/usr/bin/env bash

set -e

if ! test -f vlib/v/compiler_errors_test.v; then
  echo "this script should be run in V's main repo folder!"
  exit 1
fi
  
export CURRENT_SCRIPT_PATH=$(realpath "$0")

export CC="${CC:-gcc}"
export TCC_FOLDER="${TCC_FOLDER:-thirdparty/tcc}"
export LIBGC_COMMIT="${LIBGC_COMMIT:-master}"
mkdir -p $TCC_FOLDER/lib/

echo "          CC: $CC"
echo "  TCC_FOLDER: $TCC_FOLDER"
echo "LIBGC_COMMIT: $LIBGC_COMMIT"
echo ===============================================================

rm -rf bdwgc/

pushd .
git clone https://github.com/ivmai/bdwgc
cd bdwgc/

git checkout $LIBGC_COMMIT
export LIBGC_COMMIT_FULL_HASH=$(git rev-parse HEAD)

./autogen.sh

CC=$CC CFLAGS='-Os -mtune=generic -fPIC' LDFLAGS='-Os -fPIC' ./configure \
	--disable-dependency-tracking \
	--disable-docs \
	--enable-handle-fork=yes \
	--enable-rwlock \
	--enable-threads=posix \
	--enable-static \
	--enable-shared=no \
	--enable-parallel-mark \
	--enable-single-obj-compilation \
	--enable-gc-debug \
	--with-libatomic-ops=yes \
	--enable-sigrt-signals \
	--enable-mmap

make

popd

cp bdwgc/.libs/libgc.a         $TCC_FOLDER/lib/libgc.a

date                         > $TCC_FOLDER/lib/libgc_build_on_date.txt
echo $LIBGC_COMMIT_FULL_HASH > $TCC_FOLDER/lib/libgc_build_source_hash.txt
uname -a                     > $TCC_FOLDER/lib/libgc_build_machine_uname.txt

ls -la $TCC_FOLDER/lib/libgc.a
echo "Done compiling libgc, at commit $LIBGC_COMMIT , full hash: $LIBGC_COMMIT_FULL_HASH . The static library is in $TCC_FOLDER/lib/libgc.a "
