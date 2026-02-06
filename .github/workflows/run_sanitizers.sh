#!/usr/bin/env bash

export PS4='\033[0;33m+ >>>>> (${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]:+${FUNCNAME[0]}(): }\033[0m'

set -xe

echo "    Setup environment ..."
export DEBUG_OPTS=${DEBUG_OPTS:-"-keepc -cg -cflags -fno-omit-frame-pointer"}
export VFLAGS=${VFLAGS:-"-cc clang"}
v version
v -show-c-output -cflags --version -e ';' || true

echo "    Compile and run sanitizers ..."

v ${VFLAGS} ${DEBUG_OPTS} -o v2 cmd/v -cflags -fsanitize=memory
UBSAN_OPTIONS="print_stacktrace=1:halt_on_error=1" ./v2 -o v.c cmd/v

v ${VFLAGS} ${DEBUG_OPTS} -o v3 cmd/v -cflags -fsanitize=thread
TSAN_OPTIONS="suppressions=.github/workflows/run_sanitizers_thread.suppressions" UBSAN_OPTIONS="print_stacktrace=1:halt_on_error=1" ./v3 -o v.c cmd/v

v ${VFLAGS} ${DEBUG_OPTS} -o v4 cmd/v -cflags -fsanitize=undefined
UBSAN_OPTIONS="suppressions=.github/workflows/run_sanitizers_undefined.suppressions:print_stacktrace=1:halt_on_error=1" ./v4 -o v.c cmd/v

v ${VFLAGS} ${DEBUG_OPTS} -o v5 cmd/v -cflags -fsanitize=address,pointer-compare,pointer-subtract
ASAN_OPTIONS="detect_leaks=0" UBSAN_OPTIONS="print_stacktrace=1:halt_on_error=1" ./v5 -o v.c cmd/v
