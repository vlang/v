module main

#flag @DIR/probe.c
#include "@DIR/probe.h"

fn C.aligned_alloc_probe() i32

fn main() {
	code := C.aligned_alloc_probe()
	if code != 0 { panic('aligned_alloc probe failed: ${code}') }
	println('freestanding aligned_alloc ok')
}
