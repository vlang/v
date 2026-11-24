module main

#flag -I @VEXEROOT/thirdparty/libgc/include
#flag @DIR/retention.c
#flag linux -lpthread
#include "@DIR/retention.h"

fn C.persistent_payload_create(voidptr) i32
fn C.persistent_payload_collect(voidptr, i32) i32

fn main() {
	parent := vcalloc_heap(int(sizeof(voidptr)))
	if C.persistent_payload_create(parent) != 0 { panic('collector worker setup failed') }
	if C.persistent_payload_collect(parent, 0) != 0 { panic('persistent heap lost scanned child') }
	// Prove that the collection oracle can finalize the same child after clearing its only reference.
	if C.persistent_payload_collect(parent, 1) != 0 { panic('collector negative control failed') }
	println('persistent GC retention ok')
}
