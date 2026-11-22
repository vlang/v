#!/bin/sh
# Runs tcc_termux_builtins_test.c with one C compiler against the
# __builtin_bswap16/32/64 and __builtin_object_size definitions that
# tcc-termux-stdlimits.diff adds to tinycc's include/tccdefs.h.
#
# Usage: check_tcc_termux_builtins.sh [compiler]
# The compiler defaults to $CC, then cc. CFLAGS is passed to the compiler.
#
# By default the definitions are taken from the patch text, so any C compiler
# that accepts statement expressions checks them. With USE_COMPILER_DEFS=1 the
# test uses the compiler's own definitions instead: run it that way with a tcc
# built from the patched tree to check what that tcc predefines, or with GCC
# to check the test's expectations against GCC's builtins.
# PATCH overrides the patch file, to show the test fails on an older patch.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
patch=${PATCH:-"$here/../tcc-termux-stdlimits.diff"}
cc=${1:-${CC:-cc}}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
if [ "${USE_COMPILER_DEFS:-0}" = 1 ]; then
	: > "$work/shim.h"
else
	sed -n \
		-e 's/^+[[:space:]]*\(#define __builtin_bswap[0-9]*(.*\)$/\1/p' \
		-e 's/^+[[:space:]]*\(#define __builtin_object_size(.*\)$/\1/p' \
		"$patch" > "$work/shim.h"
	if [ "$(wc -l < "$work/shim.h")" -ne 4 ]; then
		echo "expected 4 __builtin_ definitions in $patch, found:" >&2
		cat "$work/shim.h" >&2
		exit 1
	fi
fi
# CFLAGS is split into words on purpose.
# shellcheck disable=SC2086
"$cc" ${CFLAGS:-} -I"$work" -o "$work/test" "$here/tcc_termux_builtins_test.c"
"$work/test"
