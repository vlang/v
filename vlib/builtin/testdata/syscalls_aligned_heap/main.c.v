module main

#include "@DIR/heap.h"
#flag @DIR/heap.c

fn C.syscalls_heap_live() uint
fn C.syscalls_c_reallocate(voidptr, uint) voidptr
fn C.syscalls_c_free(voidptr)

fn require_heap(condition bool) { if !condition { exit(94) } }

fn cycle() {
	mut original := [513]u8{}
	for i in 0 .. original.len { original[i] = u8(i & 255) }
	for alignment in [1, 2, 4, 8, 16, 64, 4096, 65536]! {
		for size in [1, 3, 12, 65, 257, 513]! {
			duplicate := unsafe { memdup_align(&original[0], size, alignment) }
			require_heap(duplicate != unsafe { nil } && uint(duplicate) % uint(alignment) == 0)
			for i in 0 .. size { require_heap(unsafe { &u8(duplicate)[i] } == original[i]) }
			// realloc may change alignment but must preserve this payload and
			// the shared C/V allocation ownership. Both free routes must work.
			resized := C.syscalls_c_reallocate(duplicate, uint(size + 19))
			require_heap(resized != unsafe { nil })
			for i in 0 .. size { require_heap(unsafe { &u8(resized)[i] } == original[i]) }
			unsafe { free(resized) }
			second := unsafe { memdup_align(&original[0], size, alignment) }
			require_heap(second != unsafe { nil } && uint(second) % uint(alignment) == 0)
			C.syscalls_c_free(second)
		}
	}
}

fn worker() {
	for _ in 0 .. 50 { cycle() }
}

fn main() {
	cycle()
	before := C.syscalls_heap_live()
	for _ in 0 .. 1000 { cycle() }
	after := C.syscalls_heap_live()
	require_heap(after <= before + 65536)
	mut workers := []thread{}
	for _ in 0 .. 8 { workers << spawn worker() }
	for handle in workers { handle.wait() }
	println('syscalls aligned heap ok')
}
