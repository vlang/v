module math

// binary_cross_entropy computes -[y log(p) + (1-y) log(1-p)].
pub fn binary_cross_entropy(y f64, p f64) f64 {
	eps := 1e-12
	pp := if p < eps {
		eps
	} else if p > 1 - eps {
		1 - eps
	} else {
		p
	}
	return -(y * math.log(pp) + (1 - y) * math.log(1 - pp))
}
