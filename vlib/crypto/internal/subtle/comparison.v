module subtle

// constant_time_byte_eq returns 1 when x == y.
pub fn constant_time_byte_eq(x byte, y byte) int {
	return int((u32(x ^ y) - 1) >> 31)
}

// constant_time_eq returns 1 when x == y.
pub fn constant_time_eq(x int, y int) int {
	return int((u64(u32(x ^ y)) - 1) >> 63)
}

// constant_time_select returns x when v == 1, and y when v == 0.
// it is undefined when v is any other value
pub fn constant_time_select(v int, x int, y int) int {
	return (~(v - 1) & x) | ((v - 1) & y)
}

// constant_time_compare returns 1 when x and y have equal contents.
// The runtime of this function is proportional of the length of x and y.
// It is *NOT* dependent on their content.
pub fn constant_time_compare(x []u8, y []u8) int {
	if x.len != y.len {
		return 0
	}
	mut v := u8(0)
	for i in 0 .. x.len {
		v |= x[i] ^ y[i]
	}
	return constant_time_byte_eq(v, 0)
}

// constant_time_copy copies the contents of y into x, when v == 1.
// When v == 0, x is left unchanged. this function is undefined, when
// v takes any other value
pub fn constant_time_copy(v int, mut x []u8, y []u8) {
	if x.len != y.len {
		panic('subtle: arrays have different lengths')
	}
	xmask := u8(v - 1)
	ymask := u8(~(v - 1))
	for i := 0; i < x.len; i++ {
		x[i] = x[i] & xmask | y[i] & ymask
	}
}

// constant_time_less_or_eq returns 1 if x <= y, and 0 otherwise.
// it is undefined when x or y are negative, or > (2^32 - 1)
pub fn constant_time_less_or_eq(x int, y int) int {
	x32 := int(x)
	y32 := int(y)
	return int(((x32 - y32 - 1) >> 31) & 1)
}
