module math

// diff returns discrete differences: out[i] = values[i+1] - values[i].
pub fn diff(values []f64) []f64 {
	if values.len < 2 {
		return []f64{}
	}
	mut out := []f64{len: values.len - 1}
	for i in 0 .. values.len - 1 {
		out[i] = values[i + 1] - values[i]
	}
	return out
}
