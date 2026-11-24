module main

#flag -Wl,--wrap=free
#flag @DIR/release.c
#include "@DIR/release.h"

fn C.heap_track_release(voidptr)
fn C.heap_release_count() i32

fn main() {
	scope := unsafe { prealloc_scope_begin() }
	heap := vcalloc_heap(4096)
	C.heap_track_release(heap)
	unsafe { vfree_heap(heap) }
	if C.heap_release_count() != 1 { panic('heap release must reach allocator once') }
	unsafe { vfree_heap(nil) }
	if C.heap_release_count() != 1 { panic('nil release reached allocator') }
	unsafe { prealloc_scope_end(scope) }
	if C.heap_release_count() != 1 { panic('scope cleanup released heap twice') }
	println('persistent release ok')
}
