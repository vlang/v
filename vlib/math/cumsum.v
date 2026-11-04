module math

// cumsum returns cumulative sums of `values`.
pub fn cumsum(values []f64) []f64 {
	mut out := []f64{len: values.len}
	mut s := 0.0
	for i, v in values {
		s += v
		out[i] = s
	}
	return out
}

// cumprod returns cumulative products of `values`.
pub fn cumprod(values []f64) []f64 {
	mut out := []f64{len: values.len}
	mut p := 1.0
	for i, v in values {
		p *= v
		out[i] = p
	}
	return out
}
