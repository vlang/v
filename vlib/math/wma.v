module math

// wma returns weighted moving averages with given weights.
pub fn wma(values []f64, weights []f64) []f64 {
	if values.len == 0 || weights.len == 0 || values.len < weights.len {
		return []f64{}
	}
	mut out := []f64{}
	mut wsum := 0.0
	for w in weights {
		wsum += w
	}
	for i in weights.len - 1 .. values.len {
		mut s := 0.0
		for j in 0 .. weights.len {
			s += values[i - (weights.len - 1) + j] * weights[j]
		}
		out << s / wsum
	}
	return out
}
