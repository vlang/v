module math

fn test_hyperbolic_half_exponential_remains_finite() {
	// Independent 200-digit Decimal exp oracles, rounded to binary64.
	inputs := [709.0, 710.0, 710.4758600739439]
	expected := [4.109203730777486e307, 1.1169973830808555e308, 1.7976931348621744e308]
	for i, x in inputs {
		assert abs(sinh(x) / expected[i] - 1) < 8e-16
		assert abs(cosh(x) / expected[i] - 1) < 8e-16
		assert sinh(-x) == -sinh(x)
		assert cosh(-x) == cosh(x)
	}
	assert is_inf(sinh(711), 1)
	assert is_inf(sinh(-711), -1)
	assert is_inf(cosh(711), 1)
	assert is_inf(cosh(-711), 1)
	assert sinh(copysign(0, -1)) == 0
	assert signbit(sinh(copysign(0, -1)))
}
