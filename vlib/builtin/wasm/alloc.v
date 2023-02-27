[has_globals]
module builtin

// Shitty `sbrk` basic `malloc` and `free` impl
// TODO: implement pure V `walloc` later

const wasm_page_size = 64 * 1024

__global g_heap_base = isize(0)

fn init() {
	g_heap_base = __memory_grow(3)
	if g_heap_base == -1 {
		panic('g_heap_base: malloc() == nil')
	}
	g_heap_base *= wasm_page_size
}

// malloc dynamically allocates a `n` bytes block of memory on the heap.
// malloc returns a `byteptr` pointing to the memory address of the allocated space.
// unlike the `calloc` family of functions - malloc will not zero the memory block.
[unsafe]
pub fn malloc(n isize) &u8 {
	if n <= 0 {
		panic('malloc(n <= 0)')
	}

	res := g_heap_base
	g_heap_base += n

	return &u8(res)
}

// free allows for manually freeing memory allocated at the address `ptr`.
// currently does not free any memory.
[unsafe]
pub fn free(ptr voidptr) {
	_ := ptr
}

// vcalloc dynamically allocates a zeroed `n` bytes block of memory on the heap.
// vcalloc returns a `byteptr` pointing to the memory address of the allocated space.
// Unlike `v_calloc` vcalloc checks for negative values given in `n`.
[unsafe]
pub fn vcalloc(n isize) &u8 {
	if n <= 0 {
		panic('vcalloc(n <= 0)')
	} else if n == 0 {
		return &u8(0)
	}

	res := unsafe { malloc(n) }

	__memory_fill(res, 0, n)

	return res
}

// vmemcpy copies n bytes from memory area src to memory area dest.
// The memory areas **CAN** overlap. vmemcpy returns a pointer to `dest`.
[unsafe]
pub fn vmemcpy(dest voidptr, const_src voidptr, n isize) voidptr {
	__memory_copy(dest, const_src, n)
	return dest
}

// vmemmove copies n bytes from memory area src to memory area dest.
// The memory areas **CAN** overlap. vmemmove returns a pointer to `dest`.
[unsafe]
pub fn vmemmove(dest voidptr, const_src voidptr, n isize) voidptr {
	__memory_copy(dest, const_src, n)
	return dest
}

// vmemset fills the first `n` bytes of the memory area pointed to by `s`,
// with the constant byte `c`. It returns a pointer to the memory area `s`.
[unsafe]
pub fn vmemset(s voidptr, c int, n isize) voidptr {
	__memory_fill(s, c, n)
	return s
}
