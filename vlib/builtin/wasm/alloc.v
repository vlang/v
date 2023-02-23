[has_globals]
module builtin

// Shitty `sbrk` basic `malloc` and `free` impl
// TODO: implement pure V `walloc` later

const wasm_page_size = 64 * 1024
__global g_heap_base = isize(0)
__global g_heap_top = isize(0)

fn init() {
	g_heap_base = __memory_grow(3)
	if g_heap_base == -1 {
		panic('g_heap_base: malloc() == nil')
	}
	g_heap_base *= wasm_page_size
}

[unsafe]
pub fn malloc(n isize) &u8 {
	if n <= 0 {
		panic('malloc(n <= 0)')
	}

	res := g_heap_base
	g_heap_base += n

	return &u8(res)
}

[unsafe]
pub fn free(ptr voidptr) {
	_ := ptr
}

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

[unsafe]
pub fn vmemcpy(dest voidptr, const_src voidptr, n isize) voidptr {
	__memory_copy(dest, const_src, n)
	return dest
}

[unsafe]
pub fn vmemmove(dest voidptr, const_src voidptr, n isize) voidptr {
	__memory_copy(dest, const_src, n)
	return dest
}

[unsafe]
pub fn vmemset(s voidptr, c int, n isize) voidptr {
	__memory_fill(s, c, n)
	return s
}