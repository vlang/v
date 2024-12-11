import math

fn test_log_base() {
	assert math.log(math.e) == 1.0
}

fn test_log2_base() {
	assert math.log2(2.0) == 1.0
}

fn test_log10_base() {
	// Note: `assert math.log10(10.0) == 1.0` currently fails, when the pure V implementation is used with
	// `./v -exclude @vlib/math/*.c.v vlib/math/log_test.v`
	assert math.veryclose(math.log10(10.0), 1.0)
}

fn test_log1p_base() {
	assert math.log1p(math.e - 1) == 1.0
}

fn test_log_b_base() {
	assert math.log_b(2.0) == 1.0
}
