type MyInt = int

fn test_shift_operators() {
	// check that shift works with all integer types
	// as the right-hand side operand
	a := 1
	b := 1024
	i := 10
	assert b == a << i8(i)
	assert b == a << u8(i)
	assert b == a << i16(i)
	assert b == a << u16(i)
	assert b == a << int(i)
	assert b == a << u32(i)
	assert b == a << i64(i)
	assert b == a << u64(i)
	assert a == b >> i8(i)
	assert a == b >> u8(i)
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
	// check that shift-assign works with all types
	// of integers on the right-hand side
	mut e := 1
	e <<= i8(i)
	assert e == b
	e >>= i8(i)
	assert e == a
	e <<= i16(i)
	assert e == b
	e >>= i16(i)
	assert e == a
	e <<= int(i)
	assert e == b
	e >>= int(i)
	assert e == a
	mut e2 := i64(1)
	e2 <<= i64(i)
	assert e2 == b
	e2 >>= i64(i)
	assert e2 == a
	e <<= u8(i)
	assert e == b
	e >>= u8(i)
	assert e == a
	e <<= u16(i)
	assert e == b
	e >>= u16(i)
	assert e == a
	mut e3 := u64(1)
	e3 <<= u32(i)
	assert e3 == u64(b)
	e3 >>= u32(i)
	assert e == a
	e3 <<= u64(i)
	assert e3 == u64(b)
	e3 >>= u64(i)
	assert e3 == u64(a)
	// Test shifts with custom int types
	x := MyInt(2)
	assert x << 2 == 8
}
