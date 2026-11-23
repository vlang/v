# WASI Preview 2 Example

Generated V bindings from WASI preview2 WIT files.

## Build

```sh
v -b wasm -component -wit hello.wit hello.v -o hello.wasm
```

## Run

Requires `wasmtime`:

```sh
wasmtime run hello.component.wasm
```

If wasmtime is not installed, the build still succeeds but running is skipped.