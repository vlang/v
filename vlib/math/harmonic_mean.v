module math

// harmonic_mean returns the harmonic mean of `values`.
pub fn harmonic_mean(values []f64) f64 {
	if values.len == 0 {
		return 0
	}
	mut s := 0.0
	for v in values {
		s += 1.0 / v
	}
	return f64(values.len) / s
}
