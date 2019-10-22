const (
	a = 3
	u = u64(1)
)

fn test_const() {
	b := (true && true) || false
	assert b == true
	assert a == 3
	assert u == u64(1)
	assert u == 1 // make sure this works without the cast
}

fn test_float_equal_operator() {
	mut a := f32(1)
	a += f32(0.000001)
	a -= f32(0.000001)
	assert a == 1
	assert !a.eqbit(1)
	assert !(a != 1)
	assert a.nebit(1)
	a += f32(0.000001)
	assert !(a < 1)
	assert !a.ltbit(1)
	assert !(a <= 1)
	assert !a.lebit(1)
	assert a > 1
	assert a.gtbit(1)
	assert a >= 1
	assert a.gebit(1)

	mut b:= f64(1)
	b += 0.000001
	b -= 0.000001
	assert b == 1
	assert !b.eqbit(1)
	assert !(b != 1)
	b += 0.000001
	assert !(b < 1)
	assert !b.ltbit(1)
	assert !(b <= 1)
	assert !b.lebit(1)
	assert b > 1
	assert b.gtbit(1)
	assert b >= 1
	assert b.gebit(1)
}

fn test_str_methods() {
	assert i8(1).str() == '1'
	assert i8(-1).str() == '-1'
	assert i16(1).str() == '1'
	assert i16(-1).str() == '-1'
	assert int(1).str() == '1'
	assert int(-1).str() == '-1'
	assert i64(1).str() == '1'
	assert i64(-1).str() == '-1'

	// assert byte(1).str() == '1'
	// assert byte(-1).str() == '255'
	assert u16(1).str() == '1'
	assert u16(-1).str() == '65535'
	assert u32(1).str() == '1'
	assert u32(-1).str() == '4294967295'
	assert u64(1).str() == '1'
	assert u64(-1).str() == '18446744073709551615'
}

fn test_implicit_conversions() {
	mut a := u32(5)
	a += 4				// int const should work
	a += byte(5)
	// a += 2.1			// float->int
	a *= 2
	assert a == byte(28)
	mut b := f64(a)
	b += u32(10)
	b += 0.125
	b += byte(2)
	// b += i64(1)		// too wide
	assert b == 40.125
	mut c := i16(b)
	c -= i8(2)
	c *= i16(-2) + i8(1)
	assert c == i16(-38)
	mut d := byte(50)
	d += 200		// implicit shortening of literal to byte
	d += byte(-5)
	assert d == 245
	mut e := i8(100)
	e += 100		// implicit shortening to i8 (warning)
	assert byte(e) == byte(200)
}

fn test_shift() {
	val := 0x13000000
	assert byte(u32(val & 0xff000000) >> 24) == 0x13
	mut val2 := byte(0x50)
	// val2 += 0x150		// overflows byte
	val2 += byte(u16(0x301) & 3)
	assert val2 == 0x51
}

/*
fn test_cmp() {
	assert 1 ≠ 2
	assert 1 ⩽ 2
	assert 1 ⩾ 0
}
*/
