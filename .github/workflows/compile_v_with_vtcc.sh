#!/usr/bin/env bash

set -ex

function show() {
	printf "\u001b[35m$1\u001b[0m\n"
}

show "Prepare"
rm -rf vtcc/

show "Clone vtcc"
.github/workflows/retry.sh git clone https://github.com/felipensp/vtcc --branch stable --quiet vtcc/
du -s vtcc/
## TODO: just `./v vtcc`, later will cause V, to detect the compiler as tcc (which it is), and add `-fwrapv`, which causes the vtcc compiler to panic currently
show "Compile vtcc"
cd vtcc/
v run make.vsh
cd ..

ls -la vtcc/vtcc
./vtcc/vtcc --version

show "Generate the C file, for the current V version"
./v -o vlang.c cmd/v
ls -la vlang.c

show "Compile the C file with vtcc"
./vtcc/vtcc -o v_compiled_with_vtcc vlang.c -lpthread
ls -la v_compiled_with_vtcc

show "Test the resulting V compiler"
./v_compiled_with_vtcc version

show "Compile and run hello with vtcc"
./v_compiled_with_vtcc -cc vtcc/vtcc -showcc run examples/hello_world.v

show "Remove the generated temporary files, so the script can be re-run cleanly"
rm -rf v_compiled_with_vtcc vlang.c vtcc/
