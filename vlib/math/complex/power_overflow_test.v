module complex

import math

fn test_power_product_sum_scales_overflowing_sums_of_finite_products() {
	for sign in [-1.0, 1.0] {
		assert power_product_sum(sign * 1e308, 1, sign * 1e308, 1) == math.inf(int(sign))
	}
}

fn test_complex_power_underflows_to_zero_when_real_exponent_sum_overflows() {
	z := Complex{0, -math.exp(1)}
	result := z.cpow(Complex{-1e308, -1e308})
	// Re(p * log(z)) is below -max_f64, but its imaginary part is finite.
	assert result.re == 0
	assert result.im == 0
}

fn test_complex_power_overflows_without_nan_when_real_exponent_sum_overflows() {
	z := Complex{0, -math.exp(1)}
	result := z.cpow(Complex{1e308, 1e308})
	assert math.is_inf(result.re, 0)
	assert math.is_inf(result.im, 0)
}
