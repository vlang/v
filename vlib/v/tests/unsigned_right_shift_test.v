const answer_u64 = u64(9223372036854775805)

const (
	answer_u32 = u32(2147483645)
	answer_u16 = u16(32765)
	answer_u8  = u8(125)
)

fn test_unsigned_right_shift_expr_isize_usize() {
	$if x32 {
		assert isize(-5) >>> 1 == answer_u32
		assert usize(-5) >>> 1 == answer_u32
	}
	$if x64 {
		assert isize(-5) >>> 1 == answer_u64
		assert usize(-5) >>> 1 == answer_u64
	}
}

fn test_unsigned_right_shift_expr() {
	assert i64(-5) >>> 1 == answer_u64
	assert -5 >>> 1 == answer_u32 // because int literal's size defaults to int's size, without an explicit cast
	assert int(-5) >>> 1 == answer_u32
	assert i16(-5) >>> 1 == answer_u16
	assert i8(-5) >>> 1 == answer_u8
}

fn test_unsigned_right_shift_assignment() {
	mut x, mut y, mut z := i64(-5), -5, int(-5)
	x >>>= 1
	y >>>= 1
	z >>>= 1
	assert x == answer_u64
	assert y == answer_u32
	assert z == answer_u32
}
