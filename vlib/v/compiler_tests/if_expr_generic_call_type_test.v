// regression: an if-expression whose branches call a generic function
// (e.g. math.min[T]/math.max[T]) must keep the branch value type. The v3
// transform previously lowered the if-value temporary to `int` when the
// declaration context supplied the default int target, truncating 0.05 -> 0.
module main

import math

fn test_if_expr_generic_call_keeps_float_type() {
	a := 1e-3 / 0.02
	x := if a > 0 { math.min(1.0, a) } else { 1.0 }
	assert x == 0.05
	y := if a > 0 { math.min[f64](1.0, a) } else { 1.0 }
	assert y == 0.05
	z := if a > 0 { math.max(a, 0.0) } else { 1.0 }
	assert z == 0.05
}

fn test_if_expr_generic_call_in_else_branch() {
	a := 1e-3 / 0.02
	x := if a < 0 { 0.0 } else { math.min(1.0, a) }
	assert x == 0.05
}

fn test_if_expr_non_generic_call_unaffected() {
	a := 1e-3 / 0.02
	x := if a > 0 { math.sqrt(a) } else { 1.0 }
	assert math.abs(x - 0.2236067977) < 1e-9
}
