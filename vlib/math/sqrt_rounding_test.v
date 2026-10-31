module math

fn test_sqrt_rounding_and_finite_extremes() {
	cases := [
		[u64(0x0000000000000001), 0x1e60000000000000]!,
		[u64(0x0000000000000002), 0x1e66a09e667f3bcd]!,
		[u64(0x000fffffffffffff), 0x1fffffffffffffff]!,
		[u64(0x0010000000000000), 0x2000000000000000]!,
		[u64(0x3fefffffffffffff), 0x3fefffffffffffff]!,
		[u64(0x3ff0000000000000), 0x3ff0000000000000]!,
		[u64(0x3ff0000000000001), 0x3ff0000000000000]!,
		[u64(0x4000000000000000), 0x3ff6a09e667f3bcd]!,
		[u64(0x4004000000000000), 0x3ff94c583ada5b53]!,
		[u64(0x4008000000000000), 0x3ffbb67ae8584caa]!,
		[u64(0x401921fb54442d18), 0x40040d931ff62705]!,
		[u64(0x7fefffffffffffff), 0x5fefffffffffffff]!,
	]
	for pair in cases {
		assert f64_bits(sqrt(f64_from_bits(pair[0]))) == pair[1]
	}
}

fn test_sqrt_preserves_special_values() {
	for value in [u64(0), sign_mask, uvinf] {
		assert f64_bits(sqrt(f64_from_bits(value))) == value
	}
	assert is_nan(sqrt(-1.0))
	assert is_nan(sqrt(inf(-1)))
	assert is_nan(sqrt(nan()))
}
