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
