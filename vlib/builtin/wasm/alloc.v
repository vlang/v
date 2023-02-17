module builtin

// Very basic `malloc` and `free` impl
// TODO: implement pure V `walloc` later

[unsafe]
pub fn malloc(n isize) &u8 {
	if n <= 0 {
		panic('malloc(n <= 0)')
	}

	res := __memory_grow(n)

	if res == -1 {
		panic('malloc() == nil')
	}

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