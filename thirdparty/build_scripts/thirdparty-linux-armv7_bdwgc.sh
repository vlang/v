#!/usr/bin/env bash

# NOTE: The configure step may require:
# sudo apt-get install libatomic-ops-dev

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

# The bundled TinyCC links this library into V programs, so it must not need
# anything that TinyCC's ARM linker cannot provide:
# - configure adds `-fexceptions` for GCC, and on ARM EHABI the unwind tables it
#   emits reference `__aeabi_unwind_cpp_pr0/pr1`, which TinyCC does not supply.
#   The `-fno-*` flags below come after configure's and turn the tables off.
# - `-marm`: armhf distributions default to Thumb-2, and TinyCC mishandles calls
#   from its ARM code into Thumb code.
# - `--enable-handle-fork=no`: fork handling registers `pthread_atfork` handlers,
#   and glibc's `pthread_atfork` (like `atexit`) is Thumb-2 code in
#   libc_nonshared.a: a call to it from a TinyCC-linked program lands at address 0.
#   Trade-off: without fork handling, a child forked by a multi-threaded program
#   must not allocate from the GC before it calls exec. V's own process spawning
#   forks and then execs, so it is unaffected.
CC=$CC CFLAGS='-fPIC -marm -fno-exceptions -fno-unwind-tables -fno-asynchronous-unwind-tables' \
	LDFLAGS='-fPIC' ./configure \
	--disable-dependency-tracking \
	--disable-docs \
	--enable-handle-fork=no \
	--enable-rwlock \
	--enable-threads=pthreads \
	--enable-static \
	--enable-shared=no \
	--enable-parallel-mark \
	--enable-single-obj-compilation \
	--enable-gc-debug \
	--enable-thread-local-alloc \
	--with-libatomic-ops=yes \
	--enable-sigrt-signals

make

popd

cp bdwgc/.libs/libgc.a         $TCC_FOLDER/lib/libgc.a
cp "$CURRENT_SCRIPT_PATH"      $TCC_FOLDER/lib/build_libgc.sh

date                         > $TCC_FOLDER/lib/libgc_build_on_date.txt
echo $LIBGC_COMMIT_FULL_HASH > $TCC_FOLDER/lib/libgc_build_source_hash.txt
uname -a                     > $TCC_FOLDER/lib/libgc_build_machine_uname.txt

ls -la $TCC_FOLDER/lib/libgc.a
echo "Done compiling libgc, at commit $LIBGC_COMMIT , full hash: $LIBGC_COMMIT_FULL_HASH . The static library is in $TCC_FOLDER/lib/libgc.a "
