module math

// multinomial returns n! / (k1! * k2! * ...).
pub fn multinomial(n int, ks []int) i64 {
	mut denom := i64(1)
	for k in ks {
		denom *= factorial_i64(k)
	}
	return factorial_i64(n) / denom
}

fn factorial_i64(n int) i64 {
	if n <= 1 {
		return 1
	}
	mut res := i64(1)
	for i in 2 .. n + 1 {
		res *= i64(i)
	}
	return res
}
