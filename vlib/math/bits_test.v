module math

fn test_is_finite_rejects_infinities_and_nan() {
	assert is_finite(0.0)
	assert is_finite(-0.0)
	assert is_finite(1.5)
	assert is_finite(-1.5)
	assert is_finite(max_f64)
	assert is_finite(-max_f64)
	assert !is_finite(inf(1))
	assert !is_finite(inf(-1))
	assert !is_finite(nan())
	// -nan() is a NaN with the sign bit set, a different bit pattern with the same "not finite" answer.
	assert !is_finite(-nan())
}

fn test_is_finite_accepts_smallest_and_largest_magnitudes() {
	// The smallest positive subnormal, f64_from_bits(1), is finite but far below min_normal.
	smallest_subnormal := f64_from_bits(u64(1))
	assert is_finite(smallest_subnormal)
	assert !is_inf(smallest_subnormal, 0)
	// 1.0 - 2**-53 is the largest double strictly below 1.0; still finite.
	assert is_finite(1.0 - pow(2.0, -53.0))
	// Every finite value satisfies x - x == 0, an identity that fails for both infinities and nan.
	for x in [f64(0.0), -0.0, 1.0, -1.0, 1e-300, 1e300, max_f64, -max_f64, smallest_subnormal] {
		assert is_finite(x)
		assert x - x == 0.0
	}
}

fn test_is_finite_is_the_negation_of_nan_or_inf() {
	for x in [f64(0.0), 1.0, -1.0, max_f64, inf(1), inf(-1), nan(), f64_from_bits(u64(1))] {
		assert is_finite(x) == (!is_nan(x) && !is_inf(x, 0))
	}
}

// math.normalize is the copy of math/bits.normalize that lives in the top-level math
// module; bits.normalize is covered by vlib/math/bits/bits_test.v, this one is not.
fn test_normalize_keeps_the_value_and_reports_an_exponent() {
	smallest_normal := 2.2250738585072014e-308 // 2**-1022
	for x in [f64(1.0), -1.0, 0.5, -12.75, smallest_normal, -smallest_normal, max_f64] {
		y, exp := normalize(x)
		assert y == x
		assert exp == 0
	}
}

fn test_normalize_scales_subnormals_up_by_2_pow_52() {
	smallest_normal := 2.2250738585072014e-308 // 2**-1022
	scale := pow(2.0, 52.0)
	for x in [f64(1.0e-310), -1.0e-310, 5.0e-315, f64_from_bits(u64(1))] {
		assert abs(x) < smallest_normal
		y, exp := normalize(x)
		assert exp == -52
		assert y == x * scale
		// y * 2**exp must reconstruct x exactly.
		assert y * pow(2.0, -52.0) == x
	}
}

// normalize documents that it "assumes x is finite and non-zero". Feeding it 0.0 hits
// that assumption, and it returns (0.0, -52) because |0| is below the smallest normal.
// NOTE: this is the observed behaviour of an inputs outside the documented domain, not
// a documented guarantee; it is asserted only to pin it down.
fn test_normalize_zero_is_outside_the_documented_domain() {
	y, exp := normalize(0.0)
	assert y == 0.0
	assert exp == -52
}

// is_finite tests the exponent bits; it must agree with the NaN and infinity
// predicates for every class of value, including signed zeros, subnormals and
// NaN payloads, whether the input is f64 or a widened f32.
fn test_is_finite_agrees_with_nan_and_inf() {
	bit_patterns := [u64(0), 0x8000000000000000, 1, 0x800fffffffffffff, 0x0010000000000000,
		0x3ff0000000000000, 0xbff0000000000000, 0x7fefffffffffffff, 0xffefffffffffffff,
		0x7ff0000000000000, 0xfff0000000000000, 0x7ff0000000000001, 0xfff0000000000001,
		0x7ff8000000000000, 0xfff8000000000000, 0x7fffffffffffffff, 0xffffffffffffffff]
	for bits_value in bit_patterns {
		f := f64_from_bits(bits_value)
		assert is_finite(f) == (!is_nan(f) && !is_inf(f, 0)), '${bits_value:016x}'
	}
	for bits_value in [u32(0), 0x80000000, 1, 0x7f7fffff, 0x7f800000, 0xff800000, 0x7fc00000,
		0x7f800001] {
		f := f32_from_bits(bits_value)
		assert is_finite(f) == (!is_nan(f) && !is_inf(f, 0)), '${bits_value:08x}'
	}
	assert is_finite(max_f64) && is_finite(-max_f64) && is_finite(smallest_non_zero_f64)
	assert !is_finite(inf(1)) && !is_finite(inf(-1)) && !is_finite(nan())
}
