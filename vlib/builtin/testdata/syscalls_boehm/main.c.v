module main

import os

$if !freestanding {
	$compile_error('the syscall runtime is required')
}
$if !gcboehm ? {
	$compile_error('explicit Boehm GC was disabled')
}

#include <gc.h>

fn C.getenv(const_name &char) &char
fn C.setenv(const_name &char, const_value &char, overwrite i32) i32
fn C.GC_get_heap_size() uint
fn C.GC_get_total_bytes() uint
fn C.GC_thread_is_registered() i32

@[noinline]
fn temporary_buffer(index int) u8 {
	mut data := []u8{len: 65536}
	data[index] = u8(index)
	return data[index]
}

fn worker() bool {
	if C.GC_thread_is_registered() == 0 { return false }
	mut sum := 0
	for i in 0 .. 1024 {
		sum += int(temporary_buffer(i))
	}
	return sum == 4 * 255 * 256 / 2
}

fn main() {
	$if no_system_libc ? {
		if os.setenv('V_BARE_BOEHM_ENV', 'from V', true) != 0 { exit(94) }
		from_v := C.getenv(c'V_BARE_BOEHM_ENV')
		if from_v == unsafe { nil } || unsafe { cstring_to_vstring(from_v) } != 'from V' {
			exit(95)
		}
		if C.setenv(c'V_BARE_BOEHM_ENV', c'from C', 1) != 0 || os.environ()['V_BARE_BOEHM_ENV'] != 'from C' {
			exit(96)
		}
	}
	if !gc_is_enabled() { exit(91) }
	start := C.GC_get_total_bytes()
	initial_heap := C.GC_get_heap_size()
	mut task := spawn worker()
	if !task.wait() { exit(92) }
	gc_collect()
	// More than 64 MiB has passed through this heap. Dead redraw buffers
	// must be recycled even when their owner never explicitly frees them.
	if C.GC_get_total_bytes() - start < 64 * 1024 * 1024
		|| C.GC_get_heap_size() > initial_heap + 16 * 1024 * 1024 {
		exit(93)
	}
	println('bare Boehm collected worker buffers')
}
