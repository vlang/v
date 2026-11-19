# WebAssembly backend

V's WebAssembly backend lowers the checked and transformed program to `v.ssa`, then emits a
WebAssembly binary directly. It shares SSA construction and optimization with the ARM64 backend.
`-prod` runs the SSA optimizer, including scalar local promotion and phi elimination, before
WebAssembly generation. No C compiler, Emscripten, or Binaryen is needed to emit the module.

The standard compiler builds only the C backend by default. A WebAssembly request automatically
builds and runs a cached full compiler; subsequent requests reuse it while its sources are current.
Compile a program with:

```sh
./v self
./v -b wasm -o hello.wasm examples/hello_world.v
./v -prod -b wasm -o hello.wasm examples/hello_world.v
```

Run the output with V's own WebAssembly interpreter, without an external Wasm engine:

```sh
./v run hello.wasm
./v -b wasm run examples/hello_world.v
./v run examples/wasm_codegen/add.v > add.wasm
./v wasm --invoke add add.wasm 20 22
```

`v file.wasm` also runs the binary. The default entry point is `_start`; use `--invoke` for a
named export. The [`wasm.runtime` module](../vlib/wasm/runtime/README.md) provides the same runtime
to V applications, including typed host callbacks, memory access, and configurable execution
limits. Its WASI host supplies stdio, arguments, an empty environment, and process exit.

A standalone compiler built from `vlib/v/v.v` needs `-compile-backend wasm` or `-all-backends`
when it is compiled. `-d skip_wasm` excludes WebAssembly from a compiler build.

## Embedding the same module in different hosts

`-b wasm` emits standard WebAssembly usable by browsers, Node.js, Wasmer, and other runtimes.
There is no separate `-b wasmer` backend: Wasmer loads the same binary. WASI is a set of host
imports, not a different binary format. Pure computation needs no imports; printing needs
`wasi_snapshot_preview1.fd_write`, and process exit needs `wasi_snapshot_preview1.proc_exit`.
A browser must supply any requested imports, including WASI functions if the program uses them.
The accepted `-os browser` and `-os wasi` aliases select the same wasm32 ABI; they do not supply
host services or remove imports.

The [add example](../examples/wasm/add.v) has no `main` and no host dependencies:

```v
module main

pub fn add(a int, b int) int {
	return a + b
}
```

Compile once and call the same export from Node.js and Wasmer (both print `42`):

```sh
./v -b wasm -o add.wasm examples/wasm/add.v
node examples/wasm/add.mjs add.wasm
wasmer run add.wasm --invoke add 20 22
```

In a browser, serve the module with the `application/wasm` MIME type:

```js
const { instance } = await WebAssembly.instantiateStreaming(fetch('add.wasm'), {});
console.log(instance.exports.add(20, 22)); // 42
```

Rust hosts can use Wasmer's `wasmer` crate:

```rust
use wasmer::{imports, Instance, Module, Store, TypedFunction};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut store = Store::default();
    let module = Module::from_file(&store, "add.wasm")?;
    let instance = Instance::new(&mut store, &module, &imports! {})?;
    let add: TypedFunction<(i32, i32), i32> =
        instance.exports.get_typed_function(&store, "add")?;
    assert_eq!(add.call(&mut store, 20, 22)?, 42);
    Ok(())
}
```

C hosts can load the binary using Wasmer's standard WebAssembly C API (`wasm.h`): create a
store and module, instantiate with an empty import vector, find the `add` export by name, then
use `wasm_func_call` with two `WASM_I32` arguments and one `WASM_I32` result. It is a WebAssembly
export, not a native symbol for `dlopen` or a native C function pointer. See the
[Wasmer embedding documentation](https://docs.wasmer.io/runtime/) for host API setup.

Use scalar signatures for a portable host API:

| V type | WebAssembly type | JavaScript value |
| --- | --- | --- |
| `int`, `u32`, small integers, `bool` | `i32` | `Number` (`bool`: 0 or 1) |
| `i64`, `u64` | `i64` | `BigInt` |
| `f32` | `f32` | `Number` |
| `f64` | `f64` | `Number` |
| pointers, `isize`, `usize` | `i32` | byte offset in exported `memory` |

Unsigned results retain their bits: use `value >>> 0` for a `u32` or pointer result, and
`BigInt.asUintN(64, value)` for `u64`. Aggregate layouts are experimental and should not be
used as a stable host ABI. `@[export: 'name']` gives a function a stable external name.

## Generated module and supported features

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
Runtime shifts return zero when the count is negative or reaches the operand type width.
Constants and global initializers in moduleless scripts keep the script module scope after imports.
Implicit-main scripts retain imported functions called by their top-level statements. User
functions retain their bodies when their names overlap synthetic runtime helpers.

WebAssembly uses 32-bit pointers and target-specific SSA memory layouts. Control flow is emitted
from SSA basic blocks, with parallel copies on phi edges. Unsupported operations produce a compiler
error. Aggregate language features remain experimental. Dynamic arrays of primitive elements,
structs whose fields all have a lowering, options and results are supported; maps, fixed and
nested arrays, aggregate array elements, and structs holding an unlifted field type are not, and
still produce a compiler error rather than a wrong answer.
Result values such as `!int` support both a returned value and an `error(...)`
branch, and can be unwrapped with `or { ... }`.
Reading `err` inside an `or` block produces a compile-time diagnostic. A Result's `!` can
propagate to a Result-returning caller. When `!` would panic and print the error, compilation
reports that the error value is unsupported. Use `or { ... }` to handle the failure.

Floating-point calculations are supported, but formatting a float as text remains unsupported.
Fixed float interpolation is rejected instead of returning a placeholder value.

Host functions can be declared in a `.wasm.v` file with a module import namespace:

```v
@[wasm_import_namespace: 'host']
fn WASM.read_value(address &u8, count i64) int
```

The host supplies `host.read_value` when instantiating the module. Scalar parameters and results
map to WebAssembly value types; pointers are 32-bit offsets into the module's linear memory.
Aggregate import parameters and results are rejected. Only called imports are emitted.
WebAssembly builds select `.wasm.v` sources and exclude other backend file families.

Runtime allocations use a zeroing bump allocator. Heap storage remains allocated for the module's
lifetime. Function calls use a separate 1 MiB stack in linear memory and trap if it is exhausted.
After a runtime trap, create a fresh instance before continuing execution.

The reserved export `__v_alloc(size: i32) -> i32` exposes that same allocator to the host.
It accepts an unsigned byte count, returns a 16-byte-aligned offset, zeroes the requested bytes,
and grows `memory` as needed. Oversized requests or failed growth trap. A zero-byte allocation
does not reserve storage and can return the same offset as a later allocation. There is no
individual free operation; release the instance to reclaim its memory. `memory`, `_start`, and
`__v_alloc` are reserved export names; a conflicting user export is skipped with a warning.

For buffers passed to exported functions, allocate through `__v_alloc` and write to `memory`:

```js
const e = instance.exports;
const ptr = e.__v_alloc(3) >>> 0;
new Uint8Array(e.memory.buffer, ptr, 3).set([20, 22, 7]);
```

Recreate memory views after calls that may allocate: growth can detach an old JavaScript buffer
or invalidate a native host's cached memory pointer. Do not pass native host pointers into WASM.
Hosts needing a custom allocator can reserve an arena with `__v_alloc` and manage allocations
within that arena, exposing their policy through ordinary host imports. This does not replace
V's internal allocator or let the host free V-owned objects.

For example, declare a callback in `buffer.wasm.v`:

```v
module main

@[wasm_import_namespace: 'host']
fn WASM.sum_bytes(ptr &u8, count int) int

pub fn sum_bytes(ptr &u8, count int) int {
	return WASM.sum_bytes(ptr, count)
}
```

Build with `./v -b wasm -o buffer.wasm buffer.wasm.v`, then instantiate with the named callback:

```js
let e;
const { instance } = await WebAssembly.instantiateStreaming(fetch('buffer.wasm'), {
  host: {
    sum_bytes(ptr, count) {
      return new Uint8Array(e.memory.buffer, ptr >>> 0, count).reduce((a, b) => a + b, 0);
    }
  }
});
e = instance.exports;
const ptr = e.__v_alloc(3) >>> 0;
new Uint8Array(e.memory.buffer, ptr, 3).set([20, 22, 7]);
console.log(e.sum_bytes(ptr, 3)); // 49
```

The callback above needs the instance's exported memory, so call it after instantiation, not
from a module `init()` function. A missing namespace or function causes instantiation to fail.

The focused tests instantiate generated modules with Node.js and exercise both unoptimized and
optimized SSA:

```sh
./v test vlib/v/gen/wasm/ssa_gen_test.v
V3_TEST_WASM=1 ./v vlib/v/compiler_tests/wasm_codegen_test.v
```

The compiler tests also run the add example under Wasmer when it is installed, and check host
callbacks, missing imports, memory growth, and allocations shared with V code.
