#!/usr/bin/env bash

set -e

function show() {
	printf "\u001b[35m$1\u001b[0m\n"
}

rm -rf herolib/

show "Clone https://github.com/freeflowuniverse/herolib"
v retry -- git clone --filter=blob:none --quiet https://github.com/freeflowuniverse/herolib herolib
cd herolib
git checkout development
cd cli

show "Checkout last known good commit"
git checkout fa0dac57ec218c0d65e0e8d5bba45ba2fd28d3d7

v wipe-cache
show "Build project no parallel (gcc)"
v -cc gcc -cg -enable-globals -w -n hero.v 
show "Checking build"
ls -l ./hero

v wipe-cache
show "Build project with -parallel-cc (clang)"
v -cc clang -cg -enable-globals -parallel-cc -w -n hero.v 
show "Checking gcc build"
ls -l ./hero

v wipe-cache
show "Build project with -parallel-cc (gcc)"
v -cc gcc -cg -enable-globals -parallel-cc -w -n hero.v 
show "Checking clang build"
ls -l ./hero

rm -rf ../../herolib
