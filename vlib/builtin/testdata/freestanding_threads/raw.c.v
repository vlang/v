module main

import sync.stdatomic
import time

// These are the compiler-owned C entry points, not a public V thread API.
fn C.v_bare_thread_create(fn (voidptr) voidptr, voidptr, uint, i32, &i32) voidptr
fn C.v_bare_thread_join(voidptr) voidptr
fn C.v_bare_thread_detach(voidptr)

__global raw_threads_started u64
__global raw_threads_release u64
__global raw_threads_completed u64

fn require_raw_thread(condition bool) {
	if !condition { exit(98) }
}

fn gated_worker(_argument voidptr) voidptr {
	stdatomic.add_u64(&raw_threads_started, 1)
	for stdatomic.load_u64(&raw_threads_release) == 0 {
	}
	stdatomic.add_u64(&raw_threads_completed, 1)
	return unsafe { nil }
}

fn immediate_worker(_argument voidptr) voidptr {
	stdatomic.add_u64(&raw_threads_completed, 1)
	return unsafe { nil }
}

fn main() {
	mut code := i32(-1)
	for size in [uint(1), 16383, uint(1) << 62] {
		handle := C.v_bare_thread_create(immediate_worker, unsafe { nil }, size, 0, &code)
		require_raw_thread(handle == unsafe { nil } && code != 0)
	}
	// Detach while the entry is provably still running. The worker owns every
	// mapping after the transfer, and the parent never accesses the handle again.
	handle := C.v_bare_thread_create(gated_worker, unsafe { nil }, 0, 0, &code)
	require_raw_thread(handle != unsafe { nil } && code == 0)
	for stdatomic.load_u64(&raw_threads_started) == 0 {
	}
	C.v_bare_thread_detach(handle)
	stdatomic.store_u64(&raw_threads_release, 1)
	for stdatomic.load_u64(&raw_threads_completed) != 1 {
	}
	// Exercise detach racing completion and detach after an already exited child.
	for _ in 0 .. 64 {
		late := C.v_bare_thread_create(immediate_worker, unsafe { nil }, 0, 0, &code)
		require_raw_thread(late != unsafe { nil } && code == 0)
		time.sleep(time.millisecond)
		C.v_bare_thread_detach(late)
	}
	joined := C.v_bare_thread_create(immediate_worker, unsafe { nil }, 0, 0, &code)
	require_raw_thread(joined != unsafe { nil } && code == 0)
	require_raw_thread(C.v_bare_thread_join(joined) == unsafe { nil })
	require_raw_thread(stdatomic.load_u64(&raw_threads_completed) == 66)
	println('freestanding raw thread ownership passed')
}
