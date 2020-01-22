module picohttpparser

[inline]
fn cpy_str(dst byteptr, src string) int {
	C.memcpy(dst, src.str, src.len)
	return src.len
}

[inline]
fn cpy(dst, src byteptr, len int) int {
	C.memcpy(dst, src, len)
	return len
}

[inline]
pub fn cmp(dst, src string) bool {
	if dst.len != src.len { return false }
	return C.memcmp(dst.str, src.str, src.len) == 0
}

[inline]
pub fn cmpn(dst, src string, n int) bool {
	return C.memcmp(dst.str, src.str, n) == 0
}
