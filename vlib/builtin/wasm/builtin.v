module builtin

fn __heap_base() voidptr
fn __memory_size() usize
fn __memory_grow(size usize) usize
fn __memory_fill(dest &u8, value isize, size isize)
fn __memory_copy(dest &u8, src &u8, size isize)

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

// isnil returns true if an object is nil (only for C objects).
[inline]
pub fn isnil(v voidptr) bool {
	return v == 0
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
