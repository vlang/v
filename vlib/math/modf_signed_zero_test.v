module math

fn test_modf_preserves_zero_signs() {
	for value in [0.0, -0.0, 2.0, -2.0] {
		integer, fraction := modf(value)
		assert integer == value
		assert fraction == 0.0
		assert signbit(integer) == signbit(value)
		assert signbit(fraction) == signbit(value)
	}
	for value in [0.25, -0.25] {
		integer, fraction := modf(value)
		assert integer == 0.0
		assert fraction == value
		assert signbit(integer) == signbit(value)
	}
}

fn test_log_gamma_negative_infinity() {
	assert is_inf(log_gamma(inf(-1)), 1)
	value, sign := log_gamma_sign(inf(-1))
	assert is_inf(value, 1)
	assert sign == 1
}
