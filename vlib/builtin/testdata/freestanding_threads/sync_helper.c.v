module main

import sync
import sync.stdatomic

fn C.v_sync_thread_create_detached_with_stack(voidptr, voidptr, uint) i32

__global helper_completed u64

fn increment() { stdatomic.add_u64(&helper_completed, 1) }

fn native_entry(_argument voidptr) voidptr {
	increment()
	return unsafe { nil }
}

fn main() {
	// The helper must preserve the native C error width on its failure path.
	code := C.v_sync_thread_create_detached_with_stack(voidptr(native_entry), unsafe { nil }, 1)
	if code != 22 { exit(102) }
	mut group := sync.new_waitgroup()
	for _ in 0 .. 64 { group.go(increment) }
	group.wait()
	if stdatomic.load_u64(&helper_completed) != 64 { exit(102) }
	println('freestanding sync thread helper passed')
}
