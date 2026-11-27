module math

// softmin returns -log(sum(exp(-x_i))).
pub fn softmin(x []f64) f64 {
	if x.len == 0 {
		return 0
	}
	mut m := x[0]
	for v in x {
		if v < m {
			m = v
		}
	}
	mut s := 0.0
	for v in x {
		s += math.exp(m - v)
	}
	return m - math.log(s)
}
