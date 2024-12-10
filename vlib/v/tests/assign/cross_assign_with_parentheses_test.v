import math

const ep = 1e-14

fn agm(aa f64, gg f64) f64 {
	mut a, mut g := aa, gg
	for math.abs(a - g) > math.abs(a) * ep {
		a, g = (a + g) * .5, math.sqrt(a * g)
	}
	return a
}

fn test_cross_assign_with_parentheses() {
	ret := agm(1.0, 1.0 / math.sqrt2)
	println(ret)
	assert ret == 0.8472130847939792
}
