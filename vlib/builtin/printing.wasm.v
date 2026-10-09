module builtin

// The WASM SSA backend synthesises print/println/eprint/eprintln bodies from a
// raw `write` to fd 1 (stdout) or fd 2 (stderr) and skips the source body of
// each: skip_source_fn lists these names, so build_functions drops the body and
// register_printing_stubs -> generate_print_body lowers the real one (see
// vlib/v/ssa/builder.v). These declarations exist only so the checker resolves
// the calls; their bodies are never linked on the WASM target.

// print prints a message to stdout.
pub fn print(s string) {
}

// println prints a message with a line end, to stdout.
pub fn println(s string) {
}

// eprint prints a message to stderr.
pub fn eprint(s string) {
}

// eprintln prints a message with a line end, to stderr.
pub fn eprintln(s string) {
}

// flush_stdout is a no-op on WASM: output goes straight to the fd with no stdio
// buffer to drain.
pub fn flush_stdout() {
}

// flush_stderr is a no-op on WASM: output goes straight to the fd with no stdio
// buffer to drain.
pub fn flush_stderr() {
}

// unbuffer_stdout is a no-op on WASM: output is already unbuffered.
pub fn unbuffer_stdout() {
}
