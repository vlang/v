// Regressions found by review. Every case here produced a wrong value in
// silence, so each one asserts an exact value rather than a shape.
fn test_a_wide_literal_keeps_its_top_bit() {
	// The literal widens as unsigned now. Widening it as signed turned this value
	// into 2^128 - 2^63.
	assert u128(0x8000000000000000) == u128(1) << 63
	assert i128(0x8000000000000000) == i128(1) << 63
	assert u128(18446744073709551615) == (u128(1) << 64) - u128(1)
	// A minus prefix still wraps, the way it does for the narrower types.
	assert u128(-1) == u128(0) - u128(1)
}

fn test_narrowing_casts_truncate() {
	// Every target below 64 bits used to be cast through `(int)`, which kept a
	// value that fits in 32 bits whole.
	assert u8(u128(300)) == u8(44)
	assert u16(u128(70000)) == u16(4464)
	assert int(u8(u128(300))) == 44
	assert u8(u64(300)) == u8(44)
}

fn test_mixed_width_arithmetic_keeps_its_high_bits() {
	x := (u128(1) << 100) + u128(5)
	// The sum used to be typed from the narrower operand, which cut it to 64 bits.
	assert x + u64(1) == (u128(1) << 100) + u128(6)
	assert x + 1 == (u128(1) << 100) + u128(6)
	assert x * u64(2) == (u128(1) << 101) + u128(10)
}

fn test_wide_values_survive_the_sizes_and_runes() {
	// The low 64 bits are what the sizes carry, so the test value keeps a visible
	// low-bit pattern instead of being a power of two.
	x := (u128(1) << 100) + u128(0x1122334455667788)
	assert isize(x) == isize(1234605616436508552)
	assert usize(x) == usize(1234605616436508552)
	assert rune(x) == rune(1432778632)
}
