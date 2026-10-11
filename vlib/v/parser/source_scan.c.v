module parser

// source_index_u8 returns the index of the first byte `c` of `src` at or after
// `start`, or -1 without one. The parser asks a few questions of a whole source
// before it parses it; this answers them at the speed of the C library's scan
// instead of a byte at a time.
@[inline]
fn source_index_u8(src string, c u8, start int) int {
	if start < 0 || start >= src.len {
		return -1
	}
	found := unsafe { &u8(C.memchr(src.str + start, c, usize(src.len - start))) }
	if isnil(found) {
		return -1
	}
	return int(unsafe { found - src.str })
}
