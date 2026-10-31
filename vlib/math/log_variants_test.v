import math

fn test_log_n_matches_log_quotient() {
	for x in [f64(0.25), 0.5, 2.0, math.e, 7.25, 1000.0] {
		for b in [f64(2.0), 10.0, math.e] {
			assert math.abs(math.log_n(x, b) - math.log(x) / math.log(b)) < 1e-13
		}
	}
}

fn test_log_n_same_base_is_one() {
	for b in [f64(2.0), 10.0, 0.5, math.e] {
		assert math.log_n(b, b) == 1.0
	}
}

fn test_log_n_exact_powers() {
	assert math.log_n(8.0, 2.0) == 3.0
	assert math.log_n(100.0, 10.0) == 2.0
	assert math.log_n(1.0, 7.0) == 0.0
	// log_n(27, 3) is 3 up to the last ulp
	assert math.abs(math.log_n(27.0, 3.0) - 3.0) < 1e-15
}

fn test_ilog_b_is_the_binary_exponent() {
	assert math.ilog_b(1.0) == 0
	assert math.ilog_b(1.5) == 0
	assert math.ilog_b(2.0) == 1
	assert math.ilog_b(3.9) == 1
	assert math.ilog_b(7.999) == 2
	assert math.ilog_b(8.0) == 3
	assert math.ilog_b(255.0) == 7
	assert math.ilog_b(256.0) == 8
	assert math.ilog_b(257.0) == 8
	for k in [i32(0), 1, 2, 5, 10, 40] {
		assert math.ilog_b(math.pow(2.0, f64(k))) == k
	}
}

fn test_ilog_b_documented_special_cases() {
	assert math.ilog_b(1.0) == 0
	assert math.ilog_b(2.0) == 1
	assert math.ilog_b(0.5) == -1
	assert math.ilog_b(0.0) == int(min_i32)
	assert math.ilog_b(math.nan()) == int(max_i32)
	assert math.ilog_b(math.inf(1)) == int(max_i32)
	assert math.ilog_b(math.inf(-1)) == int(max_i32)
}

fn test_cot_is_reciprocal_of_tan() {
	for x in [f64(0.1), 0.3, 0.7, 1.2, -0.4] {
		assert math.cot(x) == 1.0 / math.tan(x)
	}
}

fn test_cot_exact_values() {
	assert math.abs(math.cot(math.pi / 4) - 1.0) < 1e-14
	assert math.cot(0.0) == math.inf(1)
}

fn test_logf() {
	assert math.logf(f32(1.0)) == f32(0.0)
	assert math.abs(math.logf(f32(math.e)) - f32(1.0)) < f32(1e-7)
	assert math.abs(math.logf(f32(10.0)) - f32(2.302585)) < f32(1e-6)
}

fn test_logf_special_values() {
	assert math.logf(f32(0.0)) == f32(math.inf(-1))
	assert math.logf(f32(-0.0)) == f32(math.inf(-1))
	assert math.logf(f32(math.inf(1))) == f32(math.inf(1))
	assert math.is_nan(math.logf(f32(-1.0)))
	assert math.is_nan(math.logf(f32(math.inf(-1))))
	assert math.is_nan(math.logf(f32(math.nan())))
	for bits in [u32(1), 0x00800000, 0x7f7fffff] {
		x := math.f32_from_bits(bits)
		assert math.abs(math.logf(x) - f32(math.log(f64(x)))) < f32(1e-5)
	}
}
