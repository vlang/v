#!/usr/bin/env bash

set -ex

function show() {
	printf "\u001b[35m$1\u001b[0m\n"
}

show "Prepare"
rm -rf vtcc/

show "Clone vtcc"
.github/workflows/retry.sh git clone https://github.com/medvednikov/vtcc --branch stable --quiet vtcc/
du -s vtcc/

show "Compile vtcc"
cd vtcc/
v run make.vsh
cd ..

ls -la vtcc/vtcc
./vtcc/vtcc --version

show "Generate the C file, for the current V version"
# vtcc only targets x86_64, even when the host running it is arm64.
./v -arch amd64 -o vlang.c cmd/v
ls -la vlang.c

if [[ "$(uname)" == "Darwin" ]]; then
	show "Compile the C file with vtcc (object only, vtcc targets x86_64 ELF)"
	SDK_PATH="$(xcrun --show-sdk-path)"
	# Save vtcc's diagnostics, and include the rejected generated-C declaration.
	# A line number alone is not actionable without the generated 30 MB vlang.c.
	vtcc_errors="$(mktemp)"
	if ./vtcc/vtcc -c vlang.c -o vlang.o -I"${SDK_PATH}/usr/include" 2>"$vtcc_errors"; then
		cat "$vtcc_errors" >&2
	else
		cat "$vtcc_errors" >&2
		first_error_line="$(sed -nE 's/^vlang\.c:([0-9]+): error:.*/\1/p' "$vtcc_errors" | head -n 1)"
		if [[ "$first_error_line" =~ ^[0-9]+$ ]]; then
			start=$(( first_error_line > 10 ? first_error_line - 10 : 1 ))
			end=$(( first_error_line + 10 ))
			printf 'vtcc rejected vlang.c around line %s:\n' "$first_error_line" >&2
			nl -ba vlang.c | sed -n "${start},${end}p" >&2
		fi
		rm -f "$vtcc_errors"
		exit 1
	fi
	rm -f "$vtcc_errors"
	ls -la vlang.o
	rm -rf vlang.o vlang.c vtcc/
else
	show "Compile the C file with vtcc"
	./vtcc/vtcc -o v_compiled_with_vtcc vlang.c -lpthread
	ls -la v_compiled_with_vtcc

	show "Test the resulting V compiler"
	./v_compiled_with_vtcc version

	show "Compile and run hello with vtcc"
	./v_compiled_with_vtcc -cc vtcc/vtcc -showcc run examples/hello_world.v

	show "Remove the generated temporary files, so the script can be re-run cleanly"
	rm -rf v_compiled_with_vtcc vlang.c vtcc/
fi
