module math

// geometric_mean returns the geometric mean of `values`.
pub fn geometric_mean(values []f64) f64 {
	if values.len == 0 {
		return 0
	}
	mut p := 1.0
	for v in values {
		p *= v
	}
	return math.pow(p, 1.0 / f64(values.len))
}
