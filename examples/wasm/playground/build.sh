#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/../../.." && pwd)
cd "$repo_root"

if ! command -v emcc >/dev/null 2>&1; then
    echo 'emcc is required. Install and activate the Emscripten SDK first.' >&2
    exit 1
fi
if [ ! -x ./v ]; then
    echo 'Build V in the repository root with make first.' >&2
    exit 1
fi

build_dir=examples/wasm/playground/build
mkdir -p "$build_dir"

# Use the standalone compiler entry point, without native tool dispatch or threads.
# Production mode embeds compiler resources instead of reading host source paths.
./v -prod -no-parallel -no-prealloc -gc none -compile-backend wasm \
    -d v3_no_parallel -d no_backtrace \
    -os wasm32_emscripten -arch wasm32 -o "$build_dir/compiler.c" vlib/v/v.v

# V's memory wrappers reject pointers in the first 64 KiB.
emcc "$build_dir/compiler.c" examples/wasm/playground/compiler_stubs.c \
    -I "$repo_root" -O1 -Wno-pointer-sign -sGLOBAL_BASE=65536 \
    -sMODULARIZE=1 -sEXPORT_ES6=1 -sENVIRONMENT=web,worker,node \
    -sEXPORTED_RUNTIME_METHODS=FS,ENV,callMain -sEXIT_RUNTIME=0 \
    -sALLOW_MEMORY_GROWTH=1 -sSTACK_SIZE=16777216 -sINITIAL_MEMORY=67108864 \
    --preload-file vlib/builtin@/v/vlib/builtin \
    --preload-file vlib/strings@/v/vlib/strings \
    --preload-file vlib/strconv@/v/vlib/strconv \
    --preload-file vlib/math/bits@/v/vlib/math/bits \
    --preload-file v.mod@/v/v.mod --exclude-file '*_test*' \
    -o "$build_dir/compiler.mjs"

echo 'Built the playground. Serve examples/wasm/playground over HTTP to use it.'
