module math

// prod returns the product of `values`.
pub fn prod(values []f64) f64 {
	mut p := 1.0
	for v in values {
		p *= v
	}
	return p
}
