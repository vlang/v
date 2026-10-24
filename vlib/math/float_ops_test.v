module math

fn approx(a f64, b f64) bool {
	d := a - b
	if d < 0 {
		return -d < 1e-12 * (if b < 0 { -b } else { b }) || -d < 1e-12
	}
	return d < 1e-12 * b || d < 1e-12
}

fn test_lgamma_known_values() {
	assert approx(lgamma(1.0), 0.0)
	assert approx(lgamma(5.0), 3.1780538303479458) // ln(24)
	assert approx(lgamma(0.5), 0.5723649429247001) // ln(sqrt(pi))
	assert approx(lgamma(10.0), 12.801827480081469) // ln(362880)
	assert lgamma(0.0) == inf(1)
	assert lgamma(-2.0) == inf(1)
	assert lgamma(nan()) != lgamma(nan())
	assert lgamma(inf(1)) == inf(1)
}

fn test_remainder_ieee() {
	assert remainder(5.0, 3.0) == -1.0 // 5/3 rounds to 2 (ties not involved)
	assert remainder(4.0, 3.0) == 1.0
	assert remainder(2.5, 2.0) == 0.5 // 1.25 -> 1
	assert remainder(3.5, 2.0) == -0.5 // 1.75 -> 2
	assert remainder(-5.0, 3.0) == 1.0
	assert remainder(1.0, 0.0) != remainder(1.0, 0.0)
	assert remainder(inf(1), 2.0) != remainder(inf(1), 2.0)
	assert remainder(2.0, inf(1)) == 2.0
}

fn test_fma_and_floor_mod() {
	assert fma(2.0, 3.0, 4.0) == 10.0
	assert fma(0.0, 1.0, 5.0) == 5.0
	assert floor_mod(-1.0, 3.0) == 2.0 // Python semantics
	assert floor_mod(5.0, 3.0) == 2.0
	assert floor_mod(-5.0, 3.0) == 1.0
	assert floor_mod(5.0, -3.0) == -1.0
	assert floor_mod(6.0, 3.0) == 0.0
	assert floor_mod(1.0, 0.0) != floor_mod(1.0, 0.0)
}

fn test_floor_div() {
	// Floored quotient, the counterpart of floor_mod (issue #31082).
	assert floor_div(-10.0, 7.0) == -2.0
	assert floor_div(10.0, -7.0) == -2.0
	assert floor_div(-10.0, -7.0) == 1.0
	assert floor_div(10.0, 7.0) == 1.0
	assert floor_div(6.0, 3.0) == 2.0
	assert floor_div(-6.0, 3.0) == -2.0
	assert floor_div(5.5, 2.0) == 2.0
	assert floor_div(-5.5, 2.0) == -3.0
	// Quotient/remainder invariant: q * y + r == x.
	for x in [-10.0, -5.5, -1.0, 0.0, 1.0, 5.5, 10.0] {
		for y in [-7.0, -3.0, -1.5, 1.5, 3.0, 7.0] {
			q := floor_div(x, y)
			r := floor_mod(x, y)
			assert q * y + r == x
			if y > 0 {
				assert q * y <= x
			} else {
				assert q * y >= x
			}
		}
	}
	// Zero divisor and NaN propagate as NaN, like floor_mod.
	assert floor_div(1.0, 0.0) != floor_div(1.0, 0.0)
	assert is_nan(floor_div(1.0, 0.0))
	assert is_nan(floor_div(nan(), 1.0))
	assert is_nan(floor_div(1.0, nan()))
}

fn test_angle_conversion() {
	assert approx(to_degrees(pi), 180.0)
	assert approx(to_radians(180.0), pi)
	assert approx(to_degrees(pi / 2), 90.0)
	assert to_degrees(0.0) == 0.0
}

fn test_powers_of_two() {
	assert next_power_of_two(1) == 1
	assert next_power_of_two(2) == 2
	assert next_power_of_two(3) == 4
	assert next_power_of_two(1000) == 1024
	assert next_power_of_two(1024) == 1024
	assert next_power_of_two(0) == 1
	assert next_power_of_two(u64(1) << 62) == u64(1) << 62
	assert next_power_of_two((u64(1) << 63) + 1) == 0, '2^64 overflows u64'
	assert is_power_of_two(1)
	assert is_power_of_two(1024)
	assert !is_power_of_two(0)
	assert !is_power_of_two(1000)
	assert is_power_of_two(u64(1) << 63), '2^63 is a power of two in u64'
	assert next_power_of_two(u64(1) << 63) == u64(1) << 63
}

fn test_classify() {
	assert classify(0.0) == .zero
	assert classify(-0.0) == .zero
	assert classify(1.0) == .normal
	assert classify(-1.5) == .normal
	assert classify(nan()) == .nan
	assert classify(inf(1)) == .infinite
	assert classify(inf(-1)) == .infinite
	assert classify(1e-320) == .subnormal // below 2.2250738585072014e-308
	assert classify(2.2250738585072014e-308) == .normal
}

fn test_ieee_remainder_large_quotients_and_zero() {
	assert remainder(4294967297.5, 1.0) == -0.5
	assert remainder(-4294967297.5, 1.0) == 0.5
	assert remainder(max_f64, 1.0) == 0.0
	assert remainder(max_f64, 0.5) == 0.0, 'finite division overflow must not produce NaN'
	assert remainder(1.0, inf(1)) == 1.0
	assert signbit(remainder(-4.0, 2.0))
	assert signbit(remainder(-0.0, 3.0))
	assert !signbit(remainder(4.0, -2.0))
	assert signbit(floor_mod(4.0, -2.0))
	assert !signbit(floor_mod(-4.0, 2.0))
	assert is_nan(remainder(inf(1), 1.0))
	assert is_nan(remainder(1.0, 0.0))
}

fn test_fma_single_rounding_and_finite_overflow_cancellation() {
	x := 1.0 + exp2(-27.0)
	y := 1.0 - exp2(-27.0)
	if @BACKEND == 'c' {
		assert fma(x, y, -1.0) == -exp2(-54.0)
		assert fma(max_f64, 2.0, -max_f64) == max_f64
	}
	assert is_nan(fma(0.0, inf(1), 1.0))
	assert signbit(fma(-0.0, 2.0, -0.0))
}

fn test_lgamma_existing_special_values_and_large_inputs() {
	for x in [inf(-1), inf(1), -1000000.5, 1000000.0, -0.0000000001] {
		assert lgamma(x) == log_gamma(x)
	}
	assert is_nan(lgamma(nan()))
	assert is_inf(lgamma(-2.0), 1)
}
