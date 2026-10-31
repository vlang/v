module typed

fn C.memchr(buf &u8, c int, n uint) &u8

// index_of_lf computes a byte offset using this module's typed C return.
pub fn index_of_lf(buf []u8) int {
	base := unsafe { &u8(buf.data) }
	p := unsafe { C.memchr(base, int(`\n`), uint(buf.len)) }
	if p == unsafe { nil } { return -1 }
	return int(unsafe { p - base })
}

// return_type reports the C call type resolved by the checker.
pub fn return_type() string {
	p := unsafe { C.memchr(c'abc', int(`b`), 3) }
	return typeof(p).name
}
