#!/usr/local/bin/bash

## should be run in V's main repo folder!

rm -rf tinycc/
rm -rf thirdparty/tcc/

pushd .

git clone git://repo.or.cz/tinycc.git
cd tinycc

./configure --prefix=thirdparty/tcc \
            --bindir=thirdparty/tcc \
            --crtprefix=thirdparty/tcc/lib:/usr/lib \
            --libpaths=thirdparty/tcc/lib:/usr/lib:/lib:/usr/local/lib \
            --debug
make
make install

popd

mv tinycc/thirdparty/tcc thirdparty/tcc
mv thirdparty/tcc/tcc thirdparty/tcc/tcc.exe

thirdparty/tcc/tcc.exe -v -v
