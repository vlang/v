import math.easing

// Note: most of the checks are done not here, but by comparing the produced table,
// in the .vv/.out pair, checked by:
// `v run vlib/v/slow_tests/inout/math_easing_tables.vv > vlib/v/slow_tests/inout/math_easing_tables.out`

fn test_linear() {
	for x := -10.0; x <= 10.0; x += 0.1 {
		assert easing.linear(x) == x
	}
}

fn test_in_out_back() {
	assert easing.in_out_back(0.333).eq_epsilon(-0.04451079425639395)
	assert easing.in_out_back(3).eq_epsilon(136.79638)
}

fn test_in_out_expo() {
	assert easing.in_out_expo(123).eq_epsilon(1.0)
}

fn test_in_bounce() {
	assert easing.in_bounce(0.333).eq_epsilon(0.1382769374999998)
	assert easing.in_bounce(33).eq_epsilon(-7743)
}

fn test_in_out_bounce() {
	assert easing.in_out_bounce(0.333).eq_epsilon(0.07817887500000009)
	assert easing.in_out_bounce(33).eq_epsilon(15511)
}

fn test_out_bounce() {
	assert easing.out_bounce(0.200).eq_epsilon(0.30250000000000005)
	assert easing.out_bounce(0.600).eq_epsilon(0.7725)
	assert easing.out_bounce(0.800).eq_epsilon(0.94)
	assert easing.out_bounce(33).eq_epsilon(7767)
}
