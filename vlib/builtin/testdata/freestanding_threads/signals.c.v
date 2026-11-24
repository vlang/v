module main

import sync.stdatomic

#include "@VEXEROOT/vlib/builtin/testdata/freestanding_threads/signals.h"

fn C.v_thread_signal_install() i32
fn C.v_thread_signal_prepare_target() i32
fn C.v_thread_signal_target_was_hit() i32
fn C.v_thread_signal_hammer(i32) i32
fn C.v_thread_signal_hits() u32
fn C.v_thread_signal_failed() i32
fn C.v_bare_thread_create(fn (voidptr) voidptr, voidptr, uint, i32, &i32) voidptr

__global signal_target_tid i64

fn require_signal_thread(condition bool) {
	if !condition { exit(99) }
}

fn signal_target(_argument voidptr) voidptr {
	tid := C.v_thread_signal_prepare_target()
	require_signal_thread(tid > 0)
	stdatomic.store_i64(&signal_target_tid, i64(tid))
	for C.v_thread_signal_target_was_hit() == 0 {
	}
	return unsafe { nil }
}

fn main() {
	require_signal_thread(C.v_thread_signal_install() == 0)
	for _ in 0 .. 128 {
		stdatomic.store_i64(&signal_target_tid, 0)
		mut code := i32(-1)
		C.v_bare_thread_create(signal_target, unsafe { nil }, 0, 1, &code)
		require_signal_thread(code == 0)
		mut tid := i64(0)
		for tid == 0 { tid = stdatomic.load_i64(&signal_target_tid) }
		// Continue sending until the kernel removes this thread, including the
		// interval after its V callback returns and its TLS/stack are unmapped.
		require_signal_thread(C.v_thread_signal_hammer(i32(tid)) == 0)
	}
	require_signal_thread(C.v_thread_signal_hits() >= 128)
	require_signal_thread(C.v_thread_signal_failed() == 0)
	println('freestanding thread signal teardown passed')
}
