# WebAssembly Module Builder in V

## Description

The `wasm` module is a pure V implementation of the WebAssembly bytecode format,
designed as a builder API. It enables developers to create, manipulate, and
compile WebAssembly modules entirely within V, without external dependencies like Binaryen.
The module supports the full WebAssembly core specification, including:

- Function definitions with locals and control flow
- Multiple memories and tables
- Global variables (local and imported)
- Data and element segments
- Reference types (`funcref` and `externref`)
- Debug information via custom sections

The module generates a `[]u8` binary that can be written to a `.wasm` file or executed in memory,
making it ideal for high-performance web applications, embedded systems, or experimentation within V.

Examples are available in `examples/wasm_codegen`.

## Usage Examples

Here’s an example that demonstrates creating a module with a function:

```v
import wasm
import os

fn main() {
	mut m := wasm.Module{}

    // Define a function that adds two numbers and returns the result
    mut func := m.new_function('add', [.i32_t, .i32_t], [.i32_t])
    {
        func.local_get(0) // Push first parameter
        func.local_get(1) // Push second parameter
        func.add(.i32_t)  // Add them
    }
    m.commit(func, true) // Export as "add"

    // Compile the module to a binary
    mod := m.compile() // []u8

    os.write_file_array('add.wasm', mod) or { panic('Write failed: ${err}') }
}
```

This module does not perform verification of the WebAssembly output, yet.
Use a tool like `wasm-validate` to validate, and `wasm-dis` to show a decompiled form.
