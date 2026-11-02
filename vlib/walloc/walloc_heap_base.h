// WASM linear-memory heap base, provided by the linker (wasm-ld).
// `__heap_base` marks the end of the statically allocated data; the V port
// of walloc takes its address as the start of the preallocated heap region.
// This is a declaration only; the symbol itself is defined by the linker.
extern unsigned char __heap_base;
