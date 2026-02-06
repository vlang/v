## Description

The `wasm` module is a pure V implementation of the WebAssembly bytecode module format,
available in the form of a builder.

It allows users to generate WebAssembly modules in memory.

With the V wasm module, users can create functions, opcodes, and utilize the entire wasm
specification without the need for a large dependency like binaryen. All of this
functionality is available within V itself, making the module a valuable resource for
V developers seeking to build high-performance web applications.

The module is designed to generate a `[]u8`, which can be written to a `.wasm` file
or executed in memory.

Examples are present in `examples/wasm_codegen`.

```v
import wasm
import os

fn main() {
	mut m := wasm.Module{}
	mut func := m.new_function('add', [.i32_t, .i32_t], [.i32_t])
	{
		func.local_get(0) // | local.get 0
		func.local_get(1) // | local.get 1
		func.add(.i32_t) // | i32.add
	}
	m.commit(func, true) // `export: true`

	mod := m.compile() // []u8

	os.write_file_array('add.wasm', mod)!
}
```

This module does not perform verification of the WebAssembly output.
Use a tool like `wasm-validate` to validate, and `wasm-dis` to show a decompiled form.
