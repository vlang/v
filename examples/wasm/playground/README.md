# V WebAssembly playground

A simple browser playground that compiles and runs V locally, using an Emscripten build of
the V compiler and V's direct WebAssembly backend. No compilation server is needed.

## Build and run

Build V in the repository root (`make` if `./v` is missing, then `./v self`). Install and
activate the [Emscripten SDK](https://emscripten.org/docs/getting_started/downloads.html), so
`emcc` is on your `PATH`. From the repository root, run:

```sh
sh examples/wasm/playground/build.sh
python3 -m http.server 8000 --directory examples/wasm/playground
```

Open <http://localhost:8000>. Choose an example or edit the source, then click **Run**.
**Ctrl+Enter** or **Cmd+Enter** also runs the program. **Stop** interrupts compilation or
execution, including infinite loops. The first run downloads the compiler and its source
files; later runs can reuse the browser's HTTP cache.

The build creates `build/compiler.mjs`, `build/compiler.wasm`, and `build/compiler.data`.
Serve these generated assets together with `index.html`, `playground.js`, `worker.js`, and
`runtime.mjs`. Use HTTP rather than opening `index.html` as a local file. The playground
does not require cross-origin isolation headers, external JavaScript libraries, or a server
that executes user code.

## Supported programs

The current wasm backend supports primitive numeric and boolean expressions, functions,
conditionals, loops, and printing string literals or integers. Try the hello world, squares,
and Fibonacci examples. Strings stored in variables, arrays, maps, structs, and general
standard library programs are not yet supported by this backend. The build packages builtin
sources and their dependencies; it does not package the entire standard library.

The compiler runs without native subprocesses or threads. A small compatibility file supplies
unsupported-operation results for the host-only functions missing from Emscripten; these are
not used by the playground's compilation path.

Compiler diagnostics and program stdout/stderr appear in **Output**. Program stdout/stderr
is limited to 1 MiB. Complete output lines appear while the program is running, including
before an infinite loop. Partial lines are buffered until 4096 characters or program exit.
Each run uses a fresh worker and compiler instance, which are discarded
when execution finishes or stops.

## Tests

Run the browser runtime tests with Node.js:

```sh
node --test examples/wasm/playground/runtime_test.mjs \
  examples/wasm/playground/worker_output_test.mjs
```
