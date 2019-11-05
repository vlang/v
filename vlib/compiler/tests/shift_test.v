fn test_shift_operators() {

	// check that shift works with all integer types 
	// as the right-hand side operand
	a := 1
	b := 1024
	i := 10

	assert b == a << i8(i)
	assert b == a << byte(i)
	assert b == a << i16(i)
	assert b == a << u16(i)
	assert b == a << int(i)
	assert b == a << u32(i)
	assert b == a << i64(i)
	assert b == a << u64(i)

	assert a == b >> i8(i)
	assert a == b >> byte(i)
	assert a == b >> i16(i)
	assert a == b >> u16(i)
	assert a == b >> int(i)
	assert a == b >> u32(i)
	assert a == b >> i64(i)
	assert a == b >> u64(i)

	// check that shift operation result type is
	// the same as the type of the left-hand side operand
	mut c := u64(0)
	d := u64(1)
	c = d << i8(63)
	assert c == 9223372036854775808
}