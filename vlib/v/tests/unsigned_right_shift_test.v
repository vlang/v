fn test_unsigned_right_shift_expr() {
	assert i64(-5) >>> 1 == 9223372036854775805
	assert -5 >>> 1 == 9223372036854775805 // because type int literal's size is equals to i64
	assert int(-5) >>> 1 == 2147483645
	assert i16(-5) >>> 1 == 32765
	assert i8(-5) >>> 1 == 125
}

fn test_unsigned_right_shift_assignment() {
	mut x, mut y, mut z := i64(-5), -5, int(-5)
	x >>>= 1
	y >>>= 1
	z >>>= 1
	assert x == 9223372036854775805
	assert y == 2147483645
	assert z == 2147483645
}
