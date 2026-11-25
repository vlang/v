module main

import os
import time

#include "@DIR/probe.h"
#pkgconfig vlibc-dynamic-probe

fn C.dynamic_step(i32, voidptr) i32
fn C.dynamic_address() uint
fn C.dynamic_complete()
fn C.dynamic_completed() i32
fn C.dynamic_mapped_bytes() uint

// Keep the checks active in production, where assert statements are omitted.
fn require_dynamic(condition bool) {
	if !condition { exit(93) }
}

fn worker() uint {
	for i in 0 .. 100 {
		// The DSO takes ownership and frees this block with its C allocator.
		pointer := $if freestanding && !gcboehm ? {
			unsafe { malloc(32) }
		} $else {
			unsafe { C.malloc(32) }
		}
		require_dynamic(C.dynamic_step(i32(i), pointer) == 1)
	}
	return C.dynamic_address()
}

fn detached() {
	require_dynamic(worker() != 0)
	C.dynamic_complete()
}

fn main() {
	maps := os.read_file('/proc/self/maps') or { panic(err) }
	require_dynamic(!maps.contains('/libc.so') && !maps.contains('/libc-'))
	require_dynamic(maps.contains('libprobe.so') && maps.contains('libbase.so'))
	address := C.dynamic_address()
	mut workers := []thread uint{}
	for _ in 0 .. 24 { workers << spawn worker() }
	for handle in workers {
		require_dynamic(handle.wait() != address)
	}
	require_dynamic(C.dynamic_step(0, unsafe { nil }) == 1)
	for _ in 0 .. 128 { spawn detached() }
	deadline := time.sys_mono_now() + i64(5 * time.second)
	for C.dynamic_completed() != 128 && time.sys_mono_now() < deadline {
		time.sleep(time.millisecond)
	}
	require_dynamic(C.dynamic_completed() == 128)
	// Give detached workers time to return through their TLS/stack reclamation.
	time.sleep(50 * time.millisecond)
	// Warm the stdio/heap paths before measuring kernel mappings. Repeated
	// thread creation must not leak the loader's startup-allocator TLS blocks.
	require_dynamic(C.dynamic_mapped_bytes() != 0)
	mapped_before := C.dynamic_mapped_bytes()
	for _ in 0 .. 512 {
		handle := spawn worker()
		require_dynamic(handle.wait() != address)
	}
	mapped_after := C.dynamic_mapped_bytes()
	require_dynamic(mapped_after <= mapped_before + 256 * 1024)
	println('dynamic TLS ok')
	if os.args.len > 1 { exit(0) }
}
