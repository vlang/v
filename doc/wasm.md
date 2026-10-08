# WebAssembly backend

V's WebAssembly backend lowers the checked and transformed program to `v.ssa`, then emits a
WebAssembly binary directly. It shares SSA construction and optimization with the ARM64 backend.
`-prod` runs the SSA optimizer, including scalar local promotion and phi elimination, before
WebAssembly generation. No C compiler, Emscripten, or Binaryen is needed to emit the module.

The standard compiler includes the backend. Rebuild it, then compile a program:

```sh
./v self
./v -b wasm -o hello.wasm examples/hello_world.v
./v -prod -b wasm -o hello.wasm examples/hello_world.v
```

A standalone compiler built from `vlib/v/v.v` needs `-compile-backend wasm` or `-all-backends`
when it is compiled. `-d skip_wasm` excludes WebAssembly from a compiler build.

The output path supplied with `-o` is used exactly. Programs with `main` export a WASI `_start`
entry point and linear `memory`. Module initialization functions run before `main`, in dependency
order. Modules without `main` run their initialization functions when instantiated.

The backend supports scalar integer, boolean, and floating-point functions, calls, recursion,
branches, loops, and numeric globals. `print` and `println` support integers, booleans, and string
literals. WASI output uses
`wasi_snapshot_preview1.fd_write`. Main-module scalar functions are exported using their V names;
imported functions use their qualified module name with dots replaced by double underscores.
An explicit `@[export: 'name']` attribute supplies the export name.

Rune literals retain their full Unicode code points in expressions and global initializers.
Integer literals retain their width until an operation or comparison selects its operand types,
including full-width constants in production optimization. Numeric assignments and arguments
convert to their declared types before optimization, for both direct calls and function values.
Floating-point unary negation preserves signed zero in both unoptimized and optimized output.
Constants and global initializers in moduleless scripts keep the script module scope after imports.
Implicit-main scripts retain imported functions called by their top-level statements. User
functions retain their bodies when their names overlap synthetic runtime helpers.

WebAssembly uses 32-bit pointers and target-specific SSA memory layouts. Control flow is emitted
from SSA basic blocks, with parallel copies on phi edges. Unsupported operations produce a compiler
error. Aggregate language features remain experimental; options, results, arrays, maps, and general
struct operations are not supported by this backend.

Runtime allocations use a zeroing bump allocator. Heap storage remains allocated for the module's
lifetime. Function calls use a separate 1 MiB stack in linear memory and trap if it is exhausted.
After a runtime trap, create a fresh instance before continuing execution.

The focused tests instantiate generated modules with Node.js and exercise both unoptimized and
optimized SSA:

```sh
./v test vlib/v/gen/wasm/ssa_gen_test.v
V3_TEST_WASM=1 ./v vlib/v/compiler_tests/wasm_codegen_test.v
```
