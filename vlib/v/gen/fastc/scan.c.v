module fastc

// fastc_next_underscore uses libc's vectorized byte search to skip ordinary
// generated C text while collecting reachable function names.
@[inline]
fn fastc_next_underscore(text string, start int, end int) int {
	if start >= end {
		return end
	}
	unsafe {
		found := C.memchr(text.str + start, `_`, end - start)
		if found == voidptr(0) {
			return end
		}
		return int(&u8(found) - text.str)
	}
}
