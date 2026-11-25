module main

$if !freestanding {
	$compile_error('the syscall runtime is required')
}
$if !gcboehm ? {
	$compile_error('explicit Boehm GC was disabled')
}

#include <gc.h>

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
