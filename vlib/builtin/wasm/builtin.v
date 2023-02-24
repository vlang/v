module builtin

// WASM memory builtins
//
// Similar to `sbrk`, grow by pages.
// The size of a WebAssembly page is 64KiBs.
fn __memory_grow(size isize) isize

// `memset`
fn __memory_fill(dest &u8, value isize, size isize)

// `memcpy`
// `dest` and `src` can overlap regardless
fn __memory_copy(dest &u8, src &u8, size isize)
