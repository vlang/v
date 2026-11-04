module math

// cross_entropy computes -sum(y_i * log(p_i)).
pub fn cross_entropy(y []f64, p []f64) f64 {
	eps := 1e-12
	mut s := 0.0
	for i in 0 .. y.len {
		pp := if p[i] < eps { eps } else { p[i] }
		s -= y[i] * math.log(pp)
	}
	return s
}
