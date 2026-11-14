module math

// weighted_mean computes sum(w_i x_i)/sum(w_i).
pub fn weighted_mean(values []f64, weights []f64) f64 {
	if values.len == 0 || values.len != weights.len {
		return 0
	}
	mut s := 0.0
	mut w := 0.0
	for i in 0 .. values.len {
		s += values[i] * weights[i]
		w += weights[i]
	}
	if w == 0 {
		return 0
	}
	return s / w
}
