# V WebAssembly Component Example

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