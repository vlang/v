module builtin

// vstrlen returns the V length of the C string `s` (0 terminator is not counted).
// The C string is expected to be a &byte pointer.
[inline; unsafe]
pub fn vstrlen(s &byte) int {
	return unsafe { C.strlen(&char(s)) }
}

// vstrlen_char returns the V length of the C string `s` (0 terminator is not counted).
// The C string is expected to be a &char pointer.
[inline; unsafe]
pub fn vstrlen_char(s &char) int {
	return unsafe { C.strlen(s) }
}

// vmemcpy copies n bytes from memory area src to memory area dest.
// The memory areas *MUST NOT OVERLAP*.  Use vmemmove, if the memory
// areas do overlap. vmemcpy returns a pointer to `dest`.
[inline; unsafe]
pub fn vmemcpy(dest voidptr, const_src voidptr, n isize) voidptr {
	unsafe {
		return C.memcpy(dest, const_src, n)
	}
}

// vmemmove copies n bytes from memory area `src` to memory area `dest`.
// The memory areas *MAY* overlap: copying takes place as though the bytes
// in `src` are first copied into a temporary array that does not overlap
// `src` or `dest`, and the bytes are then copied from the temporary array
// to `dest`. vmemmove returns a pointer to `dest`.
[inline; unsafe]
pub fn vmemmove(dest voidptr, const_src voidptr, n isize) voidptr {
	unsafe {
		return C.memmove(dest, const_src, n)
	}
}

// vmemcmp compares the first n bytes (each interpreted as unsigned char)
// of the memory areas s1 and s2. It returns an integer less than, equal to,
// or greater than zero, if the first n bytes of s1 is found, respectively,
// to be less than, to match, or be greater than the first n bytes of s2.
// For a nonzero return value, the sign is determined by the sign of the
// difference between the first pair of bytes (interpreted as unsigned char)
// that differ in s1 and s2.
// If n is zero, the return value is zero.
// Do NOT use vmemcmp to compare security critical data, such as cryptographic
// secrets, because the required CPU time depends on the number of equal bytes.
// You should use a function that performs comparisons in constant time for
// this.
[inline; unsafe]
pub fn vmemcmp(const_s1 voidptr, const_s2 voidptr, n isize) int {
	unsafe {
		return C.memcmp(const_s1, const_s2, n)
	}
}

// vmemset fills the first `n` bytes of the memory area pointed to by `s`,
// with the constant byte `c`. It returns a pointer to the memory area `s`.
[inline; unsafe]
pub fn vmemset(s voidptr, c int, n isize) voidptr {
	unsafe {
		return C.memset(s, c, n)
	}
}

type FnSortCB = fn (const_a voidptr, const_b voidptr) int

[inline; unsafe]
fn vqsort(base voidptr, nmemb usize, size usize, sort_cb FnSortCB) {
	C.qsort(base, nmemb, size, voidptr(sort_cb))
}
