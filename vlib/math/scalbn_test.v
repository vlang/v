module math

// scalbn(x, n) is x * 2**n, and ldexp is documented as exactly that, so the two must
// agree bit for bit for every exponent in the normal range.
fn test_scalbn_matches_ldexp() {
	for n in [-1074, -1000, -1022, -1023, -100, -52, -1, 0, 1, 52, 100, 1000, 1022, 1023] {
		for x in [f64(1.0), -1.0, 1.5, -2.75, 0.5, 3.0] {
			assert scalbn(x, n) == ldexp(x, n)
		}
	}
}

// Powers of two must be exact: 1.0 scaled by n is exactly 2**n, and scaling by n and
// then by -n returns the original value.
fn test_scalbn_scales_by_exact_powers_of_two() {
	assert scalbn(1.0, 0) == 1.0
	assert scalbn(1.0, 1) == 2.0
	assert scalbn(1.0, 10) == 1024.0
	assert scalbn(1.0, -10) == 0.0009765625
	assert scalbn(1.0, 52) == 4.503599627370496e+15
	assert scalbn(1.0, -52) == pow(2.0, -52.0)
	for n in [-60, -20, -1, 0, 1, 20, 60] {
		assert scalbn(1.0, n) == pow(2.0, f64(n))
		assert scalbn(scalbn(1.0, n), -n) == 1.0
	}
	// The mantissa is scaled as well, so a non-power-of-two keeps its digits.
	assert scalbn(1.5, 3) == 12.0
	assert scalbn(-2.5, -4) == -0.15625
	assert scalbn(-1.0, 3) == -8.0
	assert scalbn(3.0, -1) == 1.5
	for n in [-40, -3, 0, 5, 40] {
		for x in [f64(1.25), 3.5, -7.75, 0.1] {
			assert scalbn(x, n) == x * pow(2.0, f64(n))
		}
	}
}

// Exponents past the f64 range saturate: the result overflows to +inf/-inf, matching
// the sign of the scaled value.
fn test_scalbn_overflows_to_infinity() {
	assert scalbn(1.0, 1024) == inf(1)
	assert scalbn(1.0, 2000) == inf(1)
	assert scalbn(1.0, 5000) == inf(1)
	assert scalbn(1.0, 3000) == inf(1)
	assert scalbn(-1.0, 2000) == inf(-1)
	// 2**1023 is still representable, so only 1024 and above overflow.
	assert is_finite(scalbn(1.0, 1023))
	assert !is_inf(scalbn(1.0, 1023), 0)
	assert scalbn(1.0, 1023) == 8.98846567431158e+307
	assert scalbn(1.0, 1022) == 4.49423283715579e+307
}

// Very negative exponents underflow. The smallest representable double is 2**-1074, so
// anything below that rounds to zero.
fn test_scalbn_underflows_through_the_subnormals() {
	assert scalbn(1.0, -1022) == 2.2250738585072014e-308
	assert scalbn(1.0, -1023) == 1.1125369292536007e-308
	assert scalbn(1.0, -1074) == 5e-324
	assert scalbn(1.0, -1200) == 0.0
	assert scalbn(1.0, -1075) == 0.0
	assert scalbn(1.0, -2046) == 0.0
	assert scalbn(1.0, -3100) == 0.0
	// The sign of the zero carries the sign of the input.
	assert scalbn(-1.0, -1200) == 0.0
	assert signbit(scalbn(-1.0, -1200))
}

fn test_scalbn_zero_and_infinity_pass_through() {
	assert scalbn(0.0, 5) == 0.0
	assert !signbit(scalbn(0.0, 5))
	assert signbit(scalbn(-0.0, 5))
	assert scalbn(inf(1), 5) == inf(1)
	assert scalbn(inf(-1), 5) == inf(-1)
	assert scalbn(inf(1), -5) == inf(1)
	assert is_nan(scalbn(nan(), 5))
}

// frexp and scalbn are inverse operations: f == frac * 2**exp, so scalbn of the
// decomposition must rebuild f exactly for normals and subnormals alike.
fn test_scalbn_inverts_frexp() {
	for x in [f64(1.0), -1.0, 0.5, 1.5, -12.75, 1024.0, 1e-300, 1e300, max_f64] {
		frac, exp := frexp(x)
		assert abs(frac) >= 0.5
		assert abs(frac) < 1.0
		assert scalbn(frac, exp) == x
	}
	// Zero and the infinities decompose to themselves with exponent 0.
	for x in [f64(0.0), -0.0, inf(1), inf(-1)] {
		frac, exp := frexp(x)
		assert frac == x
		assert exp == 0
	}
}
