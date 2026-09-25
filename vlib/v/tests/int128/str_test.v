// The decimal text of a 128-bit value: `str()`, println and string
// interpolation all go through the same conversion, so the text is checked here
// rather than by capturing stdout.
fn test_u128_str() {
	assert u128(0).str() == '0'
	assert u128(9).str() == '9'
	assert u128(10).str() == '10'
	assert u128(3000000000).str() == '3000000000'
	// The first value that no longer fits in 64 bits.
	assert (u128(1) << 64).str() == '18446744073709551616'
	assert ((u128(1) << 127) - u128(1) + (u128(1) << 127)).str() == '340282366920938463463374607431768211455'
}

fn test_i128_str() {
	assert i128(0).str() == '0'
	assert i128(-1).str() == '-1'
	assert i128(-128).str() == '-128'
	assert i128(123456789).str() == '123456789'
	assert ((i128(1) << 126) - i128(1) + (i128(1) << 126)).str() == '170141183460469231731687303715884105727'
	// The minimum value has no positive counterpart, so its magnitude comes from
	// the unsigned negation.
	assert (i128(-1) << 127).str() == '-170141183460469231731687303715884105728'
}

fn test_interpolation_uses_the_same_text() {
	v := u128(1) << 100
	assert '${v}' == '1267650600228229401496703205376'
	neg := i128(-1) << 63
	assert '${neg}' == '-9223372036854775808'
}

fn consume_wide(x u128) int {
	_ = x
	return -7
}

fn wide_zero() u128 {
	return u128(0)
}

fn test_str_on_a_call_keeps_the_call_type() {
	// The printer choice looked through the call for a wide operand and used the
	// 128-bit conversion on the result, so this printed 2^128 - 7 natively and
	// failed the C compile where a 128-bit value is a struct.
	x := (u128(1) << 100) + u128(5)
	assert consume_wide(x).str() == '-7'
	assert '${consume_wide(x)}' == '-7'
	assert consume_wide(wide_zero()).str() == '-7'
}

fn test_str_on_a_narrowing_cast_of_a_wide_value() {
	// The same recovery crossed this cast, which made the text of a u8 depend on
	// the width of the value it was taken from.
	x := (u128(1) << 100) + u128(5)
	assert u8(x).str() == '5'
	assert '${u8(x)}' == '5'
	assert u16(u128(70000)).str() == '4464'
}
