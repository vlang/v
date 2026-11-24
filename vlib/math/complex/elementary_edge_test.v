module complex

import math

fn assert_zero_sign(value f64, negative bool) {
	assert value == 0
	assert math.signbit(value) == negative
}

fn test_principal_sqrt_signed_zero_cuts_and_nonfinite_values() {
	negative_zero := math.copysign(0, -1)
	for y in [0.0, negative_zero] {
		r := Complex{-4, y}.sqrt()
		assert_zero_sign(r.re, false)
		assert r.im == math.copysign(2, y)
		zero := Complex{negative_zero, y}.sqrt()
		assert_zero_sign(zero.re, false)
		assert_zero_sign(zero.im, math.signbit(y))
	}
	assert Complex{math.inf(1), 1}.sqrt().re == math.inf(1)
	assert_zero_sign(Complex{math.inf(1), -1}.sqrt().im, true)
	assert Complex{math.nan(), math.inf(-1)}.sqrt().im == math.inf(-1)
	assert Complex{math.nan(), math.inf(1)}.sqrt().re == math.inf(1)
	assert math.is_nan(Complex{math.inf(-1), math.nan()}.sqrt().re)
}

fn test_elementary_cuts_retain_subnormal_signs() {
	tiny := math.f64_from_bits(1)
	for y in [tiny, -tiny, 0.0, math.copysign(0, -1)] {
		assert Complex{-22, y}.ln().im == math.copysign(math.pi, y)
		assert Complex{-math.max_f64, y}.ln().im == math.copysign(math.pi, y)
	}
	for y in [0.0, math.copysign(0, -1)] {
		asin_value := Complex{2, y}.asin()
		assert asin_value.re == math.pi / 2
		assert math.signbit(asin_value.im) == math.signbit(y)
		acos_value := Complex{2, y}.acos()
		assert_zero_sign(acos_value.re, false)
		assert math.signbit(acos_value.im) != math.signbit(y)
		assert_zero_sign(Complex{1, y}.exp().im, math.signbit(y))
	}
}

fn test_scaled_elementary_operations_retain_finite_results() {
	square_root := Complex{math.max_f64, math.max_f64}.sqrt()
	assert !math.is_inf(square_root.re, 0)
	assert !math.is_inf(square_root.im, 0)
	log_value := Complex{math.max_f64, math.max_f64}.ln()
	assert !math.is_inf(log_value.re, 0)
	assert log_value.re > math.log(math.max_f64)
	near_unit := Complex{1, 1e-20}.ln()
	assert math.abs(near_unit.re / 5e-41 - 1) < 1e-15
	for y in [1000.0, -1000.0] {
		v := Complex{1, y}.tan()
		assert v.re == 0
		assert v.im == math.copysign(1, y)
	}
	// exp(710) overflows, but exp(710)*sin(minsubnormal) does not.
	assert math.is_inf(Complex{710, math.f64_from_bits(1)}.exp().re, 1)
	assert Complex{710, math.f64_from_bits(1)}.exp().im > 0
	assert math.is_inf(Complex{1, 1}.atan().re, 0) == false
	assert Complex{0, 1}.atan().im == math.inf(1)
}

fn test_power_product_sum_avoids_overflow_underflow_and_crossed_scales() {
	large := math.ldexp(1, 1023)
	below_two := math.f64_from_bits(0x3fffffffffffffff)
	assert power_product_sum(large, 2, -large, below_two) == math.ldexp(1, 971)
	tiny := math.f64_from_bits(1)
	assert power_product_sum(tiny, 0.5, tiny, 0.5) == tiny
	assert power_product_sum(tiny, 1, 0, 1) == tiny
	assert power_product_sum(0, 1, tiny, 1) == tiny
	assert math.abs(power_product_sum(1e308, 1e-308, 1e-308, 1e308) - 2) < 1e-15
	assert_zero_sign(power_product_sum(-0.0, 1, -0.0, 1), true)
	assert Complex{math.max_f64, math.max_f64}.cpow(Complex{0.5, 0}).re < math.inf(1)
	assert Complex{tiny, tiny}.cpow(Complex{0.5, 0}).re > 0
}

fn test_real_power_keeps_extreme_roots_finite_and_agrees_with_complex_power() {
	for x in [math.max_f64, math.f64_from_bits(1)] {
		z := Complex{x, x}
		root := z.pow(0.5)
		// Analytic principal square root of x + i*x, evaluated without x*x.
		scale := math.sqrt(x)
		expected_re := scale * math.sqrt((math.sqrt(2) + 1) / 2)
		expected_im := scale * math.sqrt((math.sqrt(2) - 1) / 2)
		assert math.abs(root.re / expected_re - 1) < 1e-12
		assert math.abs(root.im / expected_im - 1) < 1e-12
		assert z.cpow(Complex{0.5, 0}) == root
	}
}
