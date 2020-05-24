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
	b := f32(1.0)
	mut a := f32(1.0)
	a += 0.000001
	a -= 0.000001
	assert a == b
	assert !a.eqbit(1.0)
	assert !(a != f32(1.0))
	assert a.nebit(f32(1.0))
	a += 0.000001
	assert !(a < 1.0)
	assert !a.ltbit(1.0)
	assert !(a <= 1)
	assert !a.lebit(1)
	assert a > 1
	assert a.gtbit(1)
	assert a >= 1
	assert a.gebit(1)
	assert -1 == 1 * -1
	assert -1.0 == 1.0 * -1.0
	a = 1
	a += 0.000001
	a -= 0.000001
	assert a == f32(1.0)
	assert !a.eqbit(f32(1.0))
	assert !(a != f32(1.0))
	a += 0.000001
	assert !(a < f32(1))
	assert !a.ltbit(f32(1))
	assert !(a <= f32(1))
	assert !a.lebit(f32(1))
	assert a > f32(1)
	assert a.gtbit(f32(1))
	assert a >= 1
	assert a.gebit(1)
	f := 1.2
	ab := int(f)
	assert ab == 1
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
	assert voidptr(-1).str() == 'ffffffffffffffff'
	assert voidptr(1).str() == '1'
	assert byteptr(-1).str() == 'ffffffffffffffff'
	assert byteptr(1).str() == '1'
}

fn test_and_precendence() {
	assert (2 & 0 == 0) == ((2 & 0) == 0)
	assert (2 & 0 != 0) == ((2 & 0) != 0)
	assert (0 & 0 >= 0) == ((0 & 0) >= 0)
	assert (0 & 0 <= 0) == ((0 & 0) <= 0)
	assert (0 & 0 < 1) == ((0 & 0) < 1)
	assert (1 & 2 > 0) == ((1 & 2) > 0)
}

fn test_or_precendence() {
	assert (1 | 0 == 0) == ((1 | 0) == 0)
	assert (1 | 0 != 1) == ((1 | 0) != 1)
	assert (1 | 0 >= 2) == ((1 | 0) >= 2)
	assert (1 | 0 <= 0) == ((1 | 0) <= 0)
	assert (1 | 0 < 0) == ((1 | 0) < 0)
	assert (1 | 0 > 1) == ((1 | 0) > 1)
}

fn test_xor_precendence() {
	assert (1 ^ 0 == 2) == ((1 ^ 0) == 2)
	assert (1 ^ 0 != 2) == ((1 ^ 0) != 2)
	assert (1 ^ 0 >= 0) == ((1 ^ 0) >= 0)
	assert (1 ^ 0 <= 1) == ((1 ^ 0) <= 1)
	assert (1 ^ 0 < 0) == ((1 ^ 0) < 0)
	assert (1 ^ 0 > 1) == ((1 ^ 0) > 1)
}

fn test_left_shift_precendence() {
	assert (2 << 4 | 3) == ((2 << 4) | 3)
	assert (2 << 4 | 3) != (2 << (4 | 3))
}

fn test_right_shift_precendence() {
	assert (256 >> 4 | 3) == ((256 >> 4) | 3)
	assert (256 >> 4 | 3) != (256 >> (4 | 3))
}

fn test_i8_print() {
	b := i8(0)
	println(b)
	c := i16(7)
	println(c)
	d := u16(6)
	println(d)
	assert true
}

/*
fn test_cmp() {
	assert 1 ≠ 2
	assert 1 ⩽ 2
	assert 1 ⩾ 0
}
*/
type MyInt int

fn test_int_alias() {
	i := MyInt(2)
	assert i + 10 == 12
}

fn test_hex() {
	x := u64(10)
	assert x.hex() == 'a'
	b := 1234
	assert b.hex() == '4d2'
	b1 := -1
	assert b1.hex() == 'ffffffff'
}

fn test_oct() {
	x1 := 0o12
	assert x1 == 10
	x2 := 00000o350
	assert x2 == 232
	x3 := 000o00073
	assert x3 == 59
	x4 := 00000000
	assert x4 == 0
	x5 := 00000195
	assert x5 == 195
	x6 := -0o744
	assert x6 == -484
	x7 := -000o000042
	assert x7 == -34
	x8 := -0000112
	assert x8 == -112
	x9 := -000
	assert x9 == 0
}

fn test_int_decl() {
	x1 := 0
	x2 := 1333
	x3 := -88955
	x4 := 2000000000
	x5 := -1999999999
	assert typeof(x1) == 'int'
	assert typeof(x2) == 'int'
	assert typeof(x3) == 'int'
	assert typeof(x4) == 'int'
	assert typeof(x5) == 'int'
	// integers are always 'int' by default
	x6 := 989898932113111
	x7 := u64(-321314588900011)
	assert typeof(x6) == 'int'
	assert typeof(x7) == 'u64'
}

fn test_int_to_hex() {
	// array hex
	st := [byte(`V`), `L`, `A`, `N`, `G`]
	assert st.hex() == '564c414e47'
	assert st.hex().len == 10
	st1 := [byte(0x41)].repeat(100)
	assert st1.hex() == '41'.repeat(100)
	// --- int to hex tests
	c0 := 12
	// 8Bit
	assert byte(0).hex() == '0'
	assert byte(c0).hex() == 'c'
	assert i8(c0).hex() == 'c'
	assert byte(127).hex() == '7f'
	assert i8(127).hex() == '7f'
	assert byte(255).hex() == 'ff'
	assert byte(-1).hex() == 'ff'
	// 16bit
	assert u16(0).hex() == '0'
	assert i16(c0).hex() == 'c'
	assert u16(c0).hex() == 'c'
	assert i16(32767).hex() == '7fff'
	assert u16(32767).hex() == '7fff'
	assert i16(-1).hex() == 'ffff'
	assert u16(65535).hex() == 'ffff'
	// 32bit
	assert u32(0).hex() == '0'
	assert c0.hex() == 'c'
	assert u32(c0).hex() == 'c'
	assert 2147483647.hex() == '7fffffff'
	assert u32(2147483647).hex() == '7fffffff'
	assert (-1).hex() == 'ffffffffffffffff'
	assert u32(4294967295).hex() == 'ffffffff'
	// 64 bit
	assert u64(0).hex() == '0'
	assert i64(c0).hex() == 'c'
	assert u64(c0).hex() == 'c'
	assert i64(9223372036854775807).hex() == '7fffffffffffffff'
	assert u64(9223372036854775807).hex() == '7fffffffffffffff'
	assert i64(-1).hex() == 'ffffffffffffffff'
	assert u64(18446744073709551615).hex() == 'ffffffffffffffff'
}
