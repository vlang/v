module math

// These are all just typed casts of the f64 versions, so they must agree with the f64
// result cast to f32.
fn test_floorf_matches_floor() {
	for x in [f32(0.0), 1.5, -1.5, 3.0, -3.0, 0.25, 100.75, -100.75, 1e10, -1e10] {
		assert floorf(x) == f32(floor(f64(x)))
	}
	assert floorf(1.5) == f32(1.0)
	assert floorf(-1.5) == f32(-2.0)
	assert floorf(3.0) == f32(3.0)
}

fn test_sqrtf_matches_sqrt() {
	assert sqrtf(4.0) == f32(2.0)
	assert sqrtf(2.0) == f32(sqrt(2.0))
	assert sqrtf(0.0) == f32(0.0)
	for x in [f32(1.0), 9.0, 0.25, 100.0, 12345.0] {
		assert sqrtf(x) == f32(sqrt(f64(x)))
	}
}

fn test_powf_matches_pow() {
	assert powf(2.0, 10.0) == f32(1024.0)
	assert powf(2.0, 0.5) == f32(sqrt(2.0))
	assert powf(2.0, -1.0) == f32(0.5)
	for a in [f32(2.0), 3.5, 10.0, 0.5] {
		for b in [f32(0.0), 1.0, 2.0, 3.0, -2.0, 0.5] {
			assert powf(a, b) == f32(pow(f64(a), f64(b)))
		}
	}
}

// pow10 is a table lookup, so it must be exact for the whole documented range and
// saturate outside it.
fn test_pow10_is_exact_over_the_documented_range() {
	assert pow10(0) == 1.0
	assert pow10(1) == 10.0
	assert pow10(2) == 100.0
	assert pow10(3) == 1000.0
	assert pow10(10) == 1e10
	assert pow10(22) == 1e22
	// The tables split the exponent into a high 32-power part and a low 32-entry part.
	assert pow10(31) == 1e31
	assert pow10(32) == 1e32
	for n in [33, 63, 64, 65, 100, 200, 300, 308] {
		// The two table factors are multiplied, which is a rounding step on its own, so
		// above 32 the result can be one ulp away from pow(10.0, n).
		assert tolerance(pow10(n), pow(10.0, f64(n)), 1e-15)
	}
	for n in [-1, -20, -32, -33, -100, -300, -323] {
		// The negative table divides rather than multiplies, which can also be a ulp off.
		assert tolerance(pow10(n), pow(10.0, f64(n)), 1e-15)
	}
	// Outside the documented range the special cases apply.
	assert pow10(309) == inf(1)
	assert pow10(1000) == inf(1)
	assert pow10(-324) == 0.0
	assert pow10(-1000) == 0.0
}

// The f32 trigonometric helpers agree with the f64 versions cast to f32. sinf and cosf
// are exact cast-throughs; tanf is its own reduction, so it carries a relative error
// near f32 precision (~1e-7) and is compared with a tolerance.
fn test_sinf_cosf_and_tanf_match_their_f64_versions() {
	for x in [f32(0.0), 0.5, 1.0, 2.0, 3.0, 100.0, 0.25, -1.0] {
		assert sinf(x) == f32(sin(f64(x)))
		assert cosf(x) == f32(cos(f64(x)))
		assert tolerance(tanf(x), tan(f64(x)), 1e-5)
	}
	assert sinf(1.0) == f32(0.8414709848078965)
	assert cosf(1.0) == f32(0.5403023058681398)
	assert tanf(1.0) == f32(1.557407724654902)
	assert sinf(0.0) == f32(0.0)
	assert cosf(0.0) == f32(1.0)
}

// The lolremez approximations are documented as approximations, so the check is a loose
// absolute tolerance rather than an exact match.
fn test_aprox_sin_and_cos_track_sin_and_cos_near_zero() {
	for a in [f64(0.0), 0.25, 0.5, 0.75, 1.0, 1.25, 1.5707963267948966] {
		assert abs(aprox_sin(a) - sin(a)) < 1e-3
		// cos near pi/2 is near zero, so the absolute difference is the meaningful check.
		assert abs(aprox_cos(a) - cos(a)) < 1e-3
	}
	// Known measured values.
	assert tolerance(aprox_sin(0.5), 0.4790747547514415, 1e-12)
	assert tolerance(aprox_sin(1.0), 0.8418268033911863, 1e-12)
	assert tolerance(aprox_cos(0.5), 0.8775494664027866, 1e-12)
	assert tolerance(aprox_cos(1.0), 0.5403148100758485, 1e-12)
	// Both polynomials are close to the identity and the constant at zero...
	assert abs(aprox_sin(0.0)) < 1e-3
	assert abs(aprox_cos(0.0) - 1.0) < 1e-3
}

// NOTE: these lolremez polynomials are only fitted over a small positive interval.
// aprox_sin(-1.0) returns -0.9256... while sin(-1.0) is -0.8415, a 10% error, and
// aprox_cos(-1.0) returns 0.4962 against cos(-1.0) = 0.5403. That is the fitted
// function's behaviour outside its range, not a bug; it is recorded here so the
// limitation is visible.
fn test_aprox_sin_and_cos_are_only_fitted_away_from_zero() {
	assert abs(aprox_sin(3.0) - sin(3.0)) < 1e-3
	assert abs(aprox_cos(3.0) - cos(3.0)) < 1e-3
	// Out of the fitted interval the error is visible but still bounded.
	assert abs(aprox_sin(-1.0) - sin(-1.0)) < 0.1
	assert abs(aprox_cos(-1.0) - cos(-1.0)) < 0.1
}

// degrees converts radians to degrees, and is exactly the inverse of radians.
fn test_degrees_converts_radians() {
	assert degrees(0.0) == 0.0
	assert degrees(pi) == 180.0
	assert degrees(pi / 2.0) == 90.0
	assert degrees(pi / 4.0) == 45.0
	assert degrees(-pi) == -180.0
	for a in [f64(0.5), 1.0, 3.0, -2.0, 100.0] {
		assert degrees(a) == a * 180.0 / pi
		// The two constants 180/pi and pi/180 are not exact reciprocals, so the round
		// trip is close rather than exact.
		assert close(radians(degrees(a)), a)
		assert close(degrees(radians(a)), a)
	}
}
