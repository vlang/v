#!/bin/bash

set -e

V=$PWD/v

if [[ -x "$V" ]]
then
	echo "The v executable exists."
else
	echo "This script should be run from the top level folder of a V repository"
	echo "i.e. the folder where your V executable is."
	exit 1
fi

BUILD=$PWD/vinix_build

echo "Creating $BUILD folder..."
rm -rf $BUILD
mkdir -p $BUILD

cd $BUILD
echo "Clone current Vinix"
git clone https://github.com/vlang/vinix.git --depth=1

cd $BUILD
echo "Clone current mlibc"
git clone https://github.com/managarm/mlibc.git --depth=1

cd $BUILD
echo "Patch mlibc for Vinix"
cd mlibc 
patch -p3 < ../vinix/patches/mlibc/mlibc.patch

cd $BUILD
echo "Install mlibc headers"
mkdir mlibc-build 
cd mlibc-build 
meson --cross-file ../vinix/cross_file.txt --prefix=/ -Dheaders_only=true ../mlibc 
ninja 
mkdir ../mlibc-headers 
DESTDIR=`realpath ../mlibc-headers` ninja install

cd $BUILD
echo "Attempt to build the Vinix kernel (debug)"
cd vinix/kernel 
make PROD=false CFLAGS="-D__vinix__ -O2 -g -pipe -I../../mlibc-headers/include" 
make clean

cd $BUILD
echo "Attempt to build the Vinix kernel (prod)"
cd vinix/kernel 
make PROD=true  CFLAGS="-D__vinix__ -O2 -g -pipe -I../../mlibc-headers/include" 
make clean

rm -rf $BUILD
