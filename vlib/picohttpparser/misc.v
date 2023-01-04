module picohttpparser

[inline; unsafe]
fn cpy(dst &u8, src &u8, len int) int {
	unsafe { C.memcpy(dst, src, len) }
	return len
}

[inline]
pub fn cmp(dst string, src string) bool {
	if dst.len != src.len {
		return false
	}
	return unsafe { C.memcmp(dst.str, src.str, src.len) == 0 }
}

[inline]
pub fn cmpn(dst string, src string, n int) bool {
	return unsafe { C.memcmp(dst.str, src.str, n) == 0 }
}
