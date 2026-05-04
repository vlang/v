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
show "Patch vtcc: fix int(0x8000_0000) overflow (felipensp/vtcc#7 / vlang/v#26853)"
## 0x8000_0000 = 2147483648 overflows V's int (max 2147483647).
## V warns now and will make this a hard error soon; TCC then rejects the generated C.
## Use -2147483648 (INT_MIN) — same bit pattern in two's complement, keeps type as int,
## so no cascading type changes needed in new_section/new_symtab/struct Section.
## Remove this patch once felipensp/vtcc#7 is merged into the stable branch.
sed -i 's/const shf_private = int(0x8000_0000)/const shf_private = -2147483648/' vtcc/src/tccelf.v

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
