import math.easing

// Note: most of the checks are done not here, but by comparing the produced table,
// in the .vv/.out pair, checked by:
// `v run vlib/v/slow_tests/inout/math_easing_tables.vv > vlib/v/slow_tests/inout/math_easing_tables.out`

fn test_linear() {
	for x := -10.0; x <= 10.0; x += 0.1 {
		assert easing.linear(x) == x
	}
}
