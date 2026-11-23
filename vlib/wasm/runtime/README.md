# WebAssembly runtime

`wasm.runtime` loads, validates, and executes WebAssembly binaries entirely in V. It has no
dependency on Node.js, Wasmer, Wasmtime, or a native WebAssembly library.

## Command line

```sh
./v -b wasm -o hello.wasm examples/hello_world.v
./v run hello.wasm
./v hello.wasm
./v -b wasm run examples/hello_world.v

./v run examples/wasm_codegen/add.v > add.wasm
./v wasm --invoke add add.wasm 20 22
```

The default entry point is `_start`. `--invoke NAME` calls a named export with scalar arguments
and prints each result on its own line. Integers accept decimal and `0x` notation. Options precede
the file; arguments after the file belong to the guest. Guest `proc_exit` supplies the process exit
code. Invalid binaries, unresolved imports, and execution traps produce a diagnostic and exit 1.

`--fuel N` limits instructions per call, including nested guest calls. The default is 100 million;
`--fuel 0` disables the instruction limit. The runner uses V's existing tool cache, so its first
invocation compiles the interpreter with V. Running a binary never invokes another Wasm engine.

## Embedding

```v
import os
import wasm.runtime

fn main() {
	module_ := runtime.load(os.read_bytes('add.wasm')!)!
	mut vm := runtime.instantiate(module_, [], runtime.Config{})!
	result := vm.call('add', [runtime.Value.i32(20), runtime.Value.i32(22)])!
	assert result[0].as_i32() == 42
}
```

Load a module once and instantiate it repeatedly. Instances have independent memory, globals,
tables, and segment state. Instantiation executes the binary's start section, if present;
`call('_start', [])` is a separate operation. `module_.exports` lists exports and
`module_.signature(name)` returns a function's parameter and result types.
`module_.imports()` lists required function imports. `get_global` and `set_global` access exports.

Supply imports as typed V callbacks. Module and field names are matched separately. Argument and
result types are checked at the host boundary; scalar values retain their exact bits.

```v
import wasm.runtime

fn double(mut vm runtime.Instance, args []runtime.Value) ![]runtime.Value {
	_ = vm
	// A callback can also access guest memory through read_memory/write_memory.
	return [runtime.Value.i32(args[0].as_i32() * 2)]
}

// Pass this list as instantiate's second argument.
imports := [runtime.HostFunction{
	module:    'host'
	name:      'double'
	signature: runtime.Signature{ parameters: [.i32], results: [.i32] }
	callback:  double
}]
```

Guest addresses are offsets into linear memory, never host pointers. `read_memory` returns a
checked copy; `write_memory` checks the entire destination range. Host callbacks are trusted V
code and decide which resources the guest can access. Calls on one instance must be sequential;
reentrant calls on the same instance return an error.

`wasm.runtime.wasi.imports(argv)` supplies the runner's WASI Preview 1 subset:
`fd_read`, `fd_write`, `proc_exit`, argument access, and an empty environment. It exposes
stdin/stdout/stderr, without filesystem or network access. `fd_read` skips empty buffers and
returns the bytes read so far on EOF, including zero if stdin is already exhausted. `proc_exit`
returns a `wasi.Exit` error to an embedding application without terminating the host process.

## Execution and supported features

The interpreter decodes instructions and validates operand types once when loading. Structured
branches become direct targets with known stack heights. Guest calls use an explicit stack;
ordinary instructions and guest calls do not allocate argument or result arrays.

Supported features include scalar i32/i64/f32/f64 operations, structured control flow, multi-value
blocks and returns, direct and indirect calls, mutable globals, one 32-bit linear memory, one
funcref table, active/passive segments, bulk memory/table operations, sign extension, and saturating
float conversions. Function imports are supported. Memory, table, and global imports, externref,
SIMD, threads, exceptions, GC, memory64, and the component model are currently rejected explicitly.

Bounds violations, integer division by zero/overflow, invalid float conversions, null indirect
calls, and `unreachable` return errors. Configurable limits bound memory, tables, operand values,
call depth, and instruction fuel. Defaults allow 256 MiB of memory, one million table entries or
stack values, and 1024 call frames. Fuel is renewed for each exported call and for instantiation's
start function. It counts guest instructions, not time spent in host callbacks or bulk operations.
Loading accepts binaries up to 64 MiB, with at most one million decoded instructions and locals
across the module. Individual vectors and validation stacks also have bounded sizes.

After a trap, call-stack state is discarded, but guest memory and globals retain any writes that
preceded it. A fresh instance restores initial state. V-generated modules may leave their own
linear-memory stack pointer modified after a trap; recreate those instances before continuing.

## Tests

```sh
./v -silent test vlib/wasm/runtime/
./v -silent cmd/tools/vwasm_test.v
```

The tests exercise generated modules and malformed binaries without an external Wasm runtime.
