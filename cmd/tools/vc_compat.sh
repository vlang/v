#!/usr/bin/env bash

# Compile the single vc snapshot with compatibility fixes for older versions.
# pipefail ensures that a failed filter cannot be hidden by a successful compiler.
set -euo pipefail

snapshot=$1
shift
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

# An optional -- separates linker inputs, which must follow the streamed source
# for linkers using --as-needed. Reset -x so object files keep their own language.
compile_args=()
while [ "$#" -gt 0 ] && [ "$1" != -- ]; do
	compile_args+=("$1")
	shift
done
if [ "$#" -gt 0 ]; then
	shift
fi

# Generated string literals can contain bytes that are not valid UTF-8.
LC_ALL=C awk -f "$script_dir/vc_compat.awk" "$snapshot" | "${compile_args[@]}" -x c - -x none "$@"
