module fallback

// contains_b uses builtin's declaration without declaring or importing another one.
pub fn contains_b(buf []u8) bool {
	return unsafe { C.memchr(buf.data, int(`b`), uint(buf.len)) } != unsafe { nil }
}

// return_type reports builtin's opaque return regardless of other modules' declarations.
pub fn return_type() string {
	p := unsafe { C.memchr(c'abc', int(`b`), 3) }
	return typeof(p).name
}
