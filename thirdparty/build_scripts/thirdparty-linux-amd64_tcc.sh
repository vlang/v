#!/usr/bin/env bash

## should be run in V's main repo folder!

export CURRENT_SCRIPT_PATH=$(realpath "$0")

export TCC_COMMIT="${TCC_COMMIT:-mob}"
export TCC_FOLDER="${TCC_FOLDER:-thirdparty/tcc.$TCC_COMMIT}"

echo "TCC_COMMIT: $TCC_COMMIT"
echo "TCC_FOLDER: $TCC_FOLDER"
echo ===============================================================

rm -rf tinycc/
rm -rf thirdparty/tcc.original/
rsync -a thirdparty/tcc/ thirdparty/tcc.original/
## rm -rf $TCC_FOLDER

pushd .

git clone git://repo.or.cz/tinycc.git

cd tinycc

git checkout $TCC_COMMIT
export TCC_COMMIT_FULL_HASH=$(git rev-parse HEAD)

## Note: crt1.o is located in:
## /usr/lib/x86_64-linux-gnu on Debian/Ubuntu
## /usr/lib64 on Redhat/CentOS
## /usr/lib on ArchLinux

./configure \
            --prefix=$TCC_FOLDER \
            --bindir=$TCC_FOLDER \
            --crtprefix=$TCC_FOLDER/lib:/usr/lib/x86_64-linux-gnu:/usr/lib64:/usr/lib:/lib/x86_64-linux-gnu:/lib \
            --libpaths=$TCC_FOLDER/lib/tcc:$TCC_FOLDER/lib:/usr/lib/x86_64-linux-gnu:/usr/lib64:/usr/lib:/lib/x86_64-linux-gnu:/lib:/usr/local/lib/x86_64-linux-gnu:/usr/local/lib \
            --cc=gcc-11 \
            --extra-cflags=-O3 \
            --config-bcheck=yes \
            --config-backtrace=yes \
            --debug

make
make install

popd

rsync -a --delete tinycc/$TCC_FOLDER/                 $TCC_FOLDER/
rsync -a          thirdparty/tcc.original/.git/       $TCC_FOLDER/.git/
rsync -a          thirdparty/tcc.original/lib/libgc*  $TCC_FOLDER/lib/
rsync -a          thirdparty/tcc.original/lib/build*  $TCC_FOLDER/lib/
rsync -a          thirdparty/tcc.original/README.md   $TCC_FOLDER/README.md
rsync -a          $CURRENT_SCRIPT_PATH                $TCC_FOLDER/build.sh
mv                $TCC_FOLDER/tcc                     $TCC_FOLDER/tcc.exe

date                                                > $TCC_FOLDER/build_on_date.txt
echo $TCC_COMMIT_FULL_HASH                          > $TCC_FOLDER/build_source_hash.txt
$TCC_FOLDER/tcc.exe --version                       > $TCC_FOLDER/build_version.txt

## show the builtin search paths for sanity checking:
$TCC_FOLDER/tcc.exe -v -v

echo "tcc commit: $TCC_COMMIT , full hash: $TCC_COMMIT_FULL_HASH . The tcc executable is ready in $TCC_FOLDER/tcc.exe "
