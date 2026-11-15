module math

fn test_dim() {
	assert dim(5, 3) == 2
	assert dim(3, 5) == 0
	assert dim(5, 5) == 0
	assert is_nan(dim(nan(), 1))
}

fn test_dim_negative_and_fractional_values() {
	assert dim(-3, -5) == 2
	assert dim(-5, -3) == 0
	assert dim(1.5, -2.25) == 3.75
	assert dim(-2.25, 1.5) == 0
}

fn test_dim_nan_in_either_operand() {
	for value in [f64(0), 1.0, -1.0, inf(1), inf(-1), nan()] {
		assert is_nan(dim(nan(), value))
		assert is_nan(dim(value, nan()))
	}
}

fn test_dim_infinities() {
	assert is_nan(dim(inf(1), inf(1)))
	assert is_nan(dim(inf(-1), inf(-1)))
	assert is_inf(dim(inf(1), inf(-1)), 1)
	assert dim(inf(-1), inf(1)) == 0
	assert is_inf(dim(inf(1), 1), 1)
	assert is_inf(dim(1, inf(-1)), 1)
	assert dim(inf(-1), 1) == 0
	assert dim(1, inf(1)) == 0
}

fn test_dim_zero_is_positive() {
	negative_zero := copysign(0.0, -1.0)
	for x in [f64(0), negative_zero] {
		for y in [f64(0), negative_zero] {
			result := dim(x, y)
			assert result == 0
			assert !signbit(result)
		}
	}
	assert !signbit(dim(-1, 1))
	assert !signbit(dim(-1, -1))
}

fn test_dim_extreme_finite_values() {
	assert is_inf(dim(max_f64, -max_f64), 1)
	assert dim(-max_f64, max_f64) == 0
	assert dim(smallest_non_zero_f64, 0) == smallest_non_zero_f64
	assert dim(0, -smallest_non_zero_f64) == smallest_non_zero_f64
}
