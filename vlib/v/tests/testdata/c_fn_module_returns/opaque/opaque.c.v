module opaque

fn C.memchr(buf voidptr, c int, n uint) voidptr

// contains_b uses this module's opaque C return.
pub fn contains_b(buf []u8) bool {
	return unsafe { C.memchr(buf.data, int(`b`), uint(buf.len)) } != unsafe { nil }
}

// return_type reports the C call type resolved by the checker.
pub fn return_type() string {
	p := unsafe { C.memchr(c'abc', int(`b`), 3) }
	return typeof(p).name
}
