#!/usr/bin/env bash

set -e

if ! test -f vlib/v/compiler_errors_test.v; then
  echo "this script should be run in V's main repo folder!"
  exit 1
fi

## ensure all output is in English, independent from the current user's local:
export LANG=C

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

export AOPS_CFLAGS=$(pkg-config atomic_ops --cflags)
export AOPS_LFLAGS=$(pkg-config atomic_ops --libs)

echo "AOPS_CFLAGS=${AOPS_CFLAGS}"
echo "AOPS_LFLAGS=${AOPS_LFLAGS}"

CC=$CC CFLAGS="-Os -mtune=generic -fPIC ${AOPS_CFLAGS}" LDFLAGS="-Os -fPIC ${AOPS_LFLAGS}" ./configure \
	--disable-dependency-tracking \
	--disable-docs \
	--enable-static=yes \
	--enable-shared=yes \
	--enable-single-obj-compilation \
	--enable-gc-debug \
	--enable-large-config \
	--enable-cplusplus \
	--with-libatomic-ops=none \
	--enable-sigrt-signals

make

cd .libs/
for dname in *.dylib; do
   echo "Post processing ${dname} ..."
   install_name_tool -id "@rpath/${dname}" $dname;
   otool -D $dname;
done

popd

################################################################################################
date                         > $TCC_FOLDER/lib/libgc_build_on_date.txt
echo $LIBGC_COMMIT_FULL_HASH > $TCC_FOLDER/lib/libgc_build_source_hash.txt
uname -a                     > $TCC_FOLDER/lib/libgc_build_machine_uname.txt
echo "$0"                    > $TCC_FOLDER/lib/libgc_build_cmd.txt

rsync -a bdwgc/.libs/         $TCC_FOLDER/lib/
ls -lad $TCC_FOLDER/lib/*

echo "Done compiling libgc, at commit $LIBGC_COMMIT , full hash: $LIBGC_COMMIT_FULL_HASH . The static library is in $TCC_FOLDER/lib/libgc.a "
