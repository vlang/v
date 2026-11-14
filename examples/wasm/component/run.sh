#!/bin/sh
set -e
V=${V:-v}
$V -b wasm -component -wit hello.wit hello.v -o hello.wasm
if command -v wasmtime >/dev/null 2>&1; then
  wasmtime run hello.component.wasm
else
  echo "wasmtime not found; skipping execution"
fi