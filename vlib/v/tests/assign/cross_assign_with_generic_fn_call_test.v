import math

fn test_cross_assign_with_generic_fn_call() {
	mut x := -1
	mut y := -2
	x, y = y, math.abs(x)
	println('${x} | ${y}')
	assert x == -2
	assert y == 1
}
