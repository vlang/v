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

fn test_str_methods() {
	assert i8(1).str() == '1'
	assert i8(-1).str() == '-1'
	assert i16(1).str() == '1'
	assert i16(-1).str() == '-1'
	assert int(1).str() == '1'
	assert int(-1).str() == '-1'
	assert int(2147483647).str() == '2147483647'
	// todo: overflow check for integers
	// assert int(2147483648).str() == '-2147483648'
	// assert int(-2147483648).str() == '-2147483648'
	assert i64(1).str() == '1'
	assert i64(-1).str() == '-1'
	assert u16(1).str() == '1'
	// assert u16(-1).str() == '65535'
	assert u32(1).str() == '1'
	// assert u32(-1).str() == '4294967295'
	assert u64(1).str() == '1'
	// assert u64(-1).str() == '18446744073709551615'
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
type MyInt = int

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
	println(b1)
	// assert b1.hex() == 'ffffffff'
	// unsigned tests
	// assert u8(12).hex() == '0c'
	// assert u8(255).hex() == 'ff'
	assert u16(65535).hex() == 'ffff'
	// assert u32(-1).hex() == 'ffffffff'
	// assert u64(-1).hex() == 'ffffffffffffffff'
	// signed tests
	// assert i8(-1).hex() == 'ff'
	assert i8(12).hex() == 'c'
	assert i16(32767).hex() == '7fff'
	assert int(2147483647).hex() == '7fffffff'
	assert i64(9223372036854775807).hex() == '7fffffffffffffff'
}

fn test_bin() {
	x1 := 0b10
	assert x1 == 2
	x2 := 0b10101010
	assert x2 == 0xAA
	x3 := -0b0000001
	assert x3 == -1
	x4 := 0b11111111
	assert x4 == 255
	x5 := u8(0b11111111)
	assert x5 == 255
	x6 := char(0b11111111)
	assert int(x6) == -1
	x7 := 0b0
	assert x7 == 0
	x8 := -0b0
	assert x8 == 0
}

fn test_oct() {
	x1 := 0o12
	assert x1 == 10
	x2 := 0o350
	assert x2 == 232
	x3 := 0o00073
	assert x3 == 59
	x4 := 0
	assert x4 == 0
	x5 := 195
	assert x5 == 195
	x6 := -0o744
	assert x6 == -484
	x7 := -0o000042
	assert x7 == -34
	x8 := -112
	assert x8 == -112
	x9 := -0
	assert x9 == 0
}

fn test_num_separator() {
	// int
	assert 100_000_0 == 1000000
	assert -2_23_4_6 == -22346

	// bin
	assert 0b0_11 == 3
	assert -0b0_100 == -4

	// oct
	assert 0o1_73 == 123
	assert -0o17_5 == -125
	assert -0o175 == -125

	// hex
	assert 0xFF == 255
	assert 0xF_F == 255

	// f32 or f64
	assert 312_2.55 == 3122.55
	assert 312_2.55 == 3122.55
}

fn test_int_decl() {
	x1 := 0
	x2 := 1333
	x3 := -88955
	x4 := 2000000000
	x5 := -1999999999
	assert typeof(x1).name == 'int'
	assert typeof(x2).name == 'int'
	assert typeof(x3).name == 'int'
	assert typeof(x4).name == 'int'
	assert typeof(x5).name == 'int'
	x7 := u64(-321314588900011)
	assert typeof(x7).name == 'u64'
}

fn test_int_to_hex() {
	// array hex
	/*
	st := [u8(`V`), `L`, `A`, `N`, `G`]
	assert st.hex() == '564c414e47'
	assert st.hex().len == 10
	st1 := [u8(0x41)].repeat(100)
	assert st1.hex() == '41'.repeat(100)*/
	// --- int to hex tests
	c0 := 12
	// 8Bit
	assert u8(0).hex() == '0'
	assert u8(c0).hex() == 'c'
	assert i8(c0).hex() == 'c'
	assert u8(127).hex() == '7f'
	assert i8(127).hex() == '7f'
	assert u8(255).hex() == 'ff'
	// assert u8(-1).hex() == 'ff'
	// 16bit
	assert u16(0).hex() == '0'
	assert i16(c0).hex() == 'c'
	assert u16(c0).hex() == 'c'
	assert i16(32767).hex() == '7fff'
	assert u16(32767).hex() == '7fff'
	// assert i16(-1).hex() == 'ffff'
	assert u16(65535).hex() == 'ffff'
	// 32bit
	assert u32(0).hex() == '0'
	assert c0.hex() == 'c'
	assert u32(c0).hex() == 'c'
	assert 2147483647.hex() == '7fffffff'
	assert u32(2147483647).hex() == '7fffffff'
	// assert (-1).hex() == 'ffffffffffffffff'
	// assert u32(4294967295).hex() == 'ffffffff'
	// 64 bit
	assert u64(0).hex() == '0'
	assert i64(c0).hex() == 'c'
	assert u64(c0).hex() == 'c'
	assert i64(9223372036854775807).hex() == '7fffffffffffffff'
	assert u64(9223372036854775807).hex() == '7fffffffffffffff'
	// assert i64(-1).hex() == 'ffffffffffffffff'
	assert u64(18446744073709551615).hex() == 'ffffffffffffffff'
}

fn test_repeat() {
	b := u8(`V`)
	assert b.repeat(5) == 'VVVVV'
	assert b.repeat(1) == b.ascii_str()
	assert b.repeat(0) == ''
}
