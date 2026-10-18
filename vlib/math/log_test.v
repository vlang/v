import math

fn test_log_base() {
	assert math.log(math.e) == 1.0
}

fn test_log2_base() {
	assert math.log2(2.0) == 1.0
}

fn test_log2_exact_powers_of_two() {
	for exponent in -1074 .. 1024 {
		x := math.ldexp(1.0, exponent)
		assert math.log2(x) == f64(exponent), 'exponent: ${exponent}'
	}
}

fn test_log2_special_values() {
	assert math.log2(0.0) == math.inf(-1)
	assert math.log2(-0.0) == math.inf(-1)
	assert math.log2(math.inf(1)) == math.inf(1)
	assert math.is_nan(math.log2(-1.0))
	assert math.is_nan(math.log2(math.inf(-1)))
	assert math.is_nan(math.log2(math.nan()))
	assert math.abs(math.log2(3.0) - 1.584962500721156) < 1e-15
}

fn test_log10_base() {
	assert math.log10(10.0) == 1.0
	assert math.log10(0.00000000000000001) == -17.0
}

fn test_log1p_base() {
	assert math.log1p(math.e - 1) == 1.0
}

fn test_log1p_inf() {
	assert math.log1p(math.inf(1)) == math.inf(1)
	assert math.log1p(-1.0) == math.inf(-1)
	assert math.is_nan(math.log1p(math.inf(-1)))
}

fn test_log_b_base() {
	assert math.log_b(2.0) == 1.0
}
