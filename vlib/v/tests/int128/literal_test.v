// Literals that need more than 64 bits. The C compiler keeps the low 64 bits of
// an oversized constant and says nothing, so every value here is checked exactly:
// a truncating implementation would return different text, not an error.
fn test_wide_literal_exactness() {
	// Reducing this value modulo 2^64 gives 4047906774079501679, which is what a
	// truncated literal used to produce.
	assert u128(31732946804115296442105984367).str() == '31732946804115296442105984367'
	assert i128(31732946804115296442105984367).str() == '31732946804115296442105984367'
}

fn test_wide_literal_boundaries() {
	assert u128(340282366920938463463374607431768211455).str() == '340282366920938463463374607431768211455'
	assert i128(-170141183460469231731687303715884105728).str() == '-170141183460469231731687303715884105728'
	assert i128(170141183460469231731687303715884105727).str() == '170141183460469231731687303715884105727'
}

fn test_wide_literal_radix_and_separators() {
	// 2^64 needs 65 bits, so this is a wide literal written in hex, with the
	// separators the scanner allows.
	assert u128(0x1_0000_0000_0000_0000).str() == '18446744073709551616'
	assert u128(0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff).str() == '340282366920938463463374607431768211455'
}

fn test_wide_literal_arithmetic() {
	v := u128(31732946804115296442105984367)
	assert v > u128(1) << 64
	assert v - v == u128(0)
	assert v / v == u128(1)
	assert v % u128(10) == u128(7)
	// A wide literal and a built-up value describe the same number.
	assert u128(1) << 64 == u128(18446744073709551616)
}
