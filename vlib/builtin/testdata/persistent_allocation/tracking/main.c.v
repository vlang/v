module main

#flag @DIR/hooks.c
#include "@DIR/hooks.h"

fn C.heap_hooks_balanced() i32

fn main() {
	ordinary := vcalloc(4093)
	persistent := vcalloc_heap(4094)
	unsafe {
		free(ordinary)
		vfree_heap(persistent)
		free(nil)
		vfree_heap(nil)
	}
	if C.heap_hooks_balanced() != 1 { panic('manual heap hooks unbalanced') }
	println('persistent tracking ok')
}
