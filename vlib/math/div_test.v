module math

// fmod takes the sign of x, not of y, and the result satisfies x == n*y + fmod(x, y)
// for some integer n.
fn test_fmod_follows_the_sign_of_the_dividend() {
	assert fmod(5.0, 3.0) == 2.0
	assert fmod(-5.0, 3.0) == -2.0
	assert fmod(5.0, -3.0) == 2.0
	assert fmod(-5.0, -3.0) == -2.0
	assert fmod(7.5, 2.5) == 0.0
	assert fmod(-7.5, 2.5) == -0.0
	assert fmod(7.5, -2.5) == 0.0
	assert fmod(-7.5, -2.5) == -0.0
}

fn test_fmod_result_is_smaller_in_magnitude_than_the_divisor() {
	for x in [f64(5.0), -5.0, 7.5, -7.5, 100.25, -100.25, 0.5, -0.5, 1e10] {
		for y in [f64(1.0), 2.0, 3.5, -2.5, 10.0] {
			r := fmod(x, y)
			assert abs(r) < abs(y)
			// sign(fmod) is the sign of x only when x is not a multiple of y.
			if r != 0.0 {
				assert signbit(r) == signbit(x)
			}
		}
	}
}

fn test_fmod_zero_dividend_gives_zero() {
	assert fmod(0.0, 3.0) == 0.0
	assert fmod(-0.0, 3.0) == 0.0
	assert fmod(0.0, -3.0) == 0.0
}

fn test_fmod_special_cases() {
	// A zero divisor is a NaN, as is a NaN or infinite dividend.
	assert is_nan(fmod(5.0, 0.0))
	assert is_nan(fmod(inf(1), 1.0))
	assert is_nan(fmod(inf(-1), 1.0))
	assert is_nan(fmod(nan(), 1.0))
	// An infinite divisor leaves a finite dividend untouched.
	assert fmod(1.0, inf(1)) == 1.0
	assert fmod(-1.0, inf(-1)) == -1.0
}

// V's integer % and fmod share the same rounding rule (truncate towards zero, remainder
// takes the dividend's sign), which angle_diff then relies on.
fn test_fmod_matches_the_integer_modulo_operator() {
	assert -5 % 3 == -2
	assert 5 % -3 == 2
	assert -5 % -3 == -2
	assert fmod(-5.0, 3.0) == f64(-5 % 3)
	assert fmod(5.0, -3.0) == f64(5 % -3)
	assert fmod(-5.0, -3.0) == f64(-5 % -3)
	// The same split on a fraction: -5.5 / 2 truncates towards -2, so the remainder
	// is -5.5 - (-2 * 2) = -1.5.
	assert fmod(-5.5, 2.0) == -1.5
	assert fmod(5.5, 2.0) == 1.5
	assert fmod(5.5, -2.0) == 1.5
}
