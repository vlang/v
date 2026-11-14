module math

// rms returns the root mean square of `values`.
pub fn rms(values []f64) f64 {
	if values.len == 0 {
		return 0
	}
	mut s := 0.0
	for v in values {
		s += v * v
	}
	return math.sqrt(s / f64(values.len))
}
