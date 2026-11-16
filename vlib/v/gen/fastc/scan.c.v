module fastc

// fastc_source_text_index searches a bounded byte string, including embedded NULs.
@[inline]
fn fastc_source_text_index(text string, needle string, start int) int {
	if start >= text.len || needle.len == 0 || needle.len > text.len - start { return -1 }
	$if windows {
		return text.index_after_(needle, start)
	} $else {
		unsafe {
			found := C.memmem(text.str + start, uint(text.len - start), needle.str, uint(needle.len))
			if found == voidptr(0) { return -1 }
			return int(&u8(found) - text.str)
		}
	}
}

// fastc_next_underscore uses libc's vectorized byte search to skip ordinary
// generated C text while collecting reachable function names.
@[inline]
fn fastc_next_underscore(text string, start int, end int) int {
	return fastc_next_source_byte(text, start, end, `_`)
}

// fastc_next_source_byte searches a bounded source range without visiting every byte in V.
@[inline]
fn fastc_next_source_byte(text string, start int, end int, value u8) int {
	if start >= end {
		return end
	}
	unsafe {
		found := C.memchr(text.str + start, value, uint(end - start))
		if found == voidptr(0) {
			return end
		}
		return int(&u8(found) - text.str)
	}
}
