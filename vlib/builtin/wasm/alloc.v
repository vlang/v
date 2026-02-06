@[has_globals]
module builtin

// Shitty `sbrk` basic `malloc` and `free` impl
// TODO: implement pure V `walloc` later

__global g_heap_base = usize(vwasm_heap_base())

// malloc dynamically allocates a `n` bytes block of memory on the heap.
// malloc returns a `byteptr` pointing to the memory address of the allocated space.
// unlike the `calloc` family of functions - malloc will not zero the memory block.
@[unsafe]
pub fn malloc(n isize) &u8 {
	if n <= 0 {
		$if no_imports ? {
			return unsafe { nil }
		} $else {
			panic('malloc(n <= 0)')
		}
	}

	res := g_heap_base
	g_heap_base += usize(n)

	return &u8(res)
}

// free allows for manually freeing memory allocated at the address `ptr`.
// currently does not free any memory.
@[unsafe]
pub fn free(ptr voidptr) {
	_ := ptr
}
